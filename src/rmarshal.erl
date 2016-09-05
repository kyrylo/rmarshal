-module(rmarshal).
-export([load/1]).

-include("marshal.hrl").

-spec load(<<_:16, _:_*8>>) -> {'ok', rterm()}.

load(<<?MARSHAL_MAJOR:8, ?MARSHAL_MINOR:8, Rest/binary>>) ->
    decode(Rest, [], maps:new()).

-spec decode(<<_:8, _:_*8>>, Decoded, SymRefs) -> {Undecoded, Decoded} when
      Decoded :: rarray(),
      SymRefs :: map(),
      Undecoded :: binary();
            (<<>>, Decoded, _SymRefs) -> {'ok', Decoded} when
      Decoded :: rarray().

decode(<<>>, Decoded, _SymRefs) ->
    {ok, Decoded};
decode(<<Type:8, Rest/binary>>, Decoded, SymRefs) ->
    {DecodedChunk, UndecodedRest, NewSymRefs} = decode_chunk(Type, Rest, SymRefs),
    decode(UndecodedRest, [DecodedChunk|Decoded], NewSymRefs).

-spec decode_chunk(Type, Undecoded, SymRefs) -> BinaryFragment when
      Type :: byte(),
      Undecoded :: binary(),
      SymRefs :: map(),
      BinaryFragment :: binfrag(rterm()).

decode_chunk(?TYPE_NIL,     Bin, SymRefs) -> {nil, Bin, SymRefs};
decode_chunk(?TYPE_TRUE,    Bin, SymRefs) -> {true, Bin, SymRefs};
decode_chunk(?TYPE_FALSE,   Bin, SymRefs) -> {false, Bin, SymRefs};
decode_chunk(?TYPE_FIXNUM,  Bin, SymRefs) ->
    {Fixnum, Undecoded} = decode_fixnum(Bin),
    {Fixnum, Undecoded, SymRefs};
decode_chunk(?TYPE_BIGNUM,  Bin, SymRefs) ->
    {Bignum, Undecoded} = decode_bignum(Bin),
    {Bignum, Undecoded, SymRefs};
decode_chunk(?TYPE_IVAR,    Bin, SymRefs) ->
    {Ivar, Undecoded} = decode_ivar(Bin),
    {Ivar, Undecoded, SymRefs};
decode_chunk(?TYPE_ARRAY,   Bin, SymRefs) -> decode_array(Bin, SymRefs);
decode_chunk(?TYPE_SYMBOL,  Bin, SymRefs) -> decode_symbol(Bin, SymRefs);
decode_chunk(?TYPE_SYMLINK, Bin, SymRefs) -> decode_symlink(Bin, SymRefs);
decode_chunk(?TYPE_FLOAT,   Bin, SymRefs) ->
    {Float, Undecoded} = decode_float(Bin),
    {Float, Undecoded, SymRefs};
decode_chunk(?TYPE_HASH,    Bin,  SymRefs) -> decode_hash(Bin, SymRefs).

-spec decode_fixnum(Undecoded) -> BinaryFragment when
      Undecoded :: binary(),
      BinaryFragment :: binfrag(fixnum()).

decode_fixnum(<<16#00, Rest/binary>>) ->
    {0, Rest};
decode_fixnum(<<16#01, X:8/unsigned-integer, Rest/binary>>) ->
    {X, Rest};
decode_fixnum(<<16#02, X:8/little-integer-unit:2, Rest/binary>>) ->
    {X, Rest};
decode_fixnum(<<16#03, X:8/little-integer-unit:3, Rest/binary>>) ->
    {X, Rest};
decode_fixnum(<<16#04, X:8/little-integer-unit:4, Rest/binary>>) ->
    {X, Rest};
decode_fixnum(<<16#fc, X:8/little-unsigned-integer-unit:4, Rest/binary>>) ->
    {X - ?INT32, Rest};
decode_fixnum(<<16#fd, X:8/little-unsigned-integer-unit:3, Rest/binary>>) ->
    {X - ?INT24, Rest};
decode_fixnum(<<16#fe, X:8/little-unsigned-integer-unit:2, Rest/binary>>) ->
    {X - ?INT16, Rest};
decode_fixnum(<<16#ff, X:8/unsigned-integer, Rest/binary>>) ->
    {X - ?CHAR, Rest};
decode_fixnum(<<X:8/signed, Rest/binary>>) when 0 < X, X =< 128 ->
    {X - ?OFFSET, Rest};
decode_fixnum(<<X:8/signed, Rest/binary>>) when -128 =< X, X < 0 ->
    {X + ?OFFSET, Rest}.

-spec decode_bignum(Bin) -> BinaryFragment when
      Bin :: binary(),
      BinaryFragment :: binfrag(bignum()).

decode_bignum(<<Sign:8/bitstring, Len:8, Rest/binary>>) ->
    {X, Bin} = split_binary(Rest, ?BIGNUM_LEN(Len)),
    Decoded = binary:decode_unsigned(X, little),
    case Sign of
        <<$+>> -> {Decoded, Bin};
        <<$->> -> {Decoded * -1, Bin}
    end.

-spec decode_ivar(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rstring()).

decode_ivar(<<?TYPE_STRING, Rest/binary>>) ->
    decode_string(Rest).

-spec decode_string(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rstring()).

decode_string(<<Len:8/integer, Rest/binary>>) ->
    {Bitstring, Encoding} = case Len =:= 0 of
        true  -> {<<>>, Rest};
        false -> split_binary(Rest, Len - ?OFFSET)
    end,
    decode_string(Bitstring, Encoding).

-spec decode_string(Bitstring, <<_:40, _:_*8>>) -> BinaryFragment when
      Bitstring :: binary(),
      BinaryFragment :: binfrag(rstring()).

%% Bitstring + Encoding. I don't know how to interpret encoding.
%% WTF is T or :/;?
decode_string(Bitstring, <<6, $:, 6, $E, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest};
decode_string(Bitstring, <<6, $;, $\t, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest};
decode_string(Bitstring, <<6, $;, $\n, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest};
decode_string(Bitstring, <<6, $;, 7, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest};
decode_string(Bitstring, <<6, $;, 6, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest};
%% decode_string(Bitstring, <<6, $:, 6, $E, $F, Rest/binary>>) ->
%%     {binary_to_list(Bitstring), Rest};
decode_string(Bitstring, <<6, $;, 0, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest}.

-spec decode_array(<<_:8, _:_*8>>, SymRefs) -> BinaryFragment when
      SymRefs :: map(),
      BinaryFragment :: binfrag(rterm()).

decode_array(<<Size:8, Rest/binary>>, SymRefs) ->
    decode_array(Size - ?OFFSET, Rest, [], SymRefs).

-spec decode_array(Size, <<_:8, _:_*8>>, Decoded, SymRefs) -> BinaryFragment when
      Size :: pos_integer(),
      Decoded :: rarray(),
      SymRefs :: map(),
      BinaryFragment :: binfrag(rterm()).

decode_array(0, Bin, Decoded, SymRefs) ->
    {lists:reverse(Decoded), Bin, SymRefs};
decode_array(_Size, <<>>, _Decoded, SymRefs) ->
    {[], <<>>, SymRefs};
decode_array(Size, <<?TYPE_ARRAY, 0, Rest/binary>>, Decoded, SymRefs) ->
    decode_array(Size - 1, Rest, [[]|Decoded], SymRefs);
decode_array(Size, <<Type:8/integer, Rest/binary>>, Decoded, SymRefs) ->
    {DecodedChunk, Bin, NewSymRefs} = decode_chunk(Type, Rest, SymRefs),
    decode_array(Size - 1, Bin, [DecodedChunk|Decoded], NewSymRefs).

-spec decode_symbol(<<_:8, _:_*8>>, SymRefs) -> BinaryFragment when
      SymRefs :: map(),
      BinaryFragment :: binfrag(rsymbol()).

decode_symbol(<<Len:8/integer, Rest/binary>>, SymRefs) ->
    {Sym, Bin} = split_binary(Rest, Len - ?OFFSET),
    Atom = list_to_atom(binary_to_list(Sym)),
    SymRefSize = maps:size(SymRefs),
    SymLink = case SymRefSize of
                  0 -> 0;
                  _ -> SymRefSize + ?OFFSET
              end,
    NewSymRefs = maps:put(SymLink, Atom, SymRefs),
    {Atom, Bin, NewSymRefs}.

-spec decode_symlink(<<_:8, _:_*8>>, SymRefs) -> BinaryFragment when
      SymRefs :: map(),
      BinaryFragment :: binfrag(rsymbol()).

decode_symlink(<<SymLink:8/integer, Rest/binary>>, SymRefs) ->
    %% Offset = case SymLink of
    %%              0 -> 0;
    %%              _ -> SymLink
    %%          end,
    {maps:get(SymLink, SymRefs), Rest, SymRefs}.

-spec decode_float(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rfloat()).

decode_float(<<Len:8/integer, Rest/binary>>) ->
    {Float, Bin} = split_binary(Rest, Len - ?OFFSET),
    {binary_to_float(Float), Bin}.

-spec decode_hash(<<_:8, _:_*8>>, SymRefs) -> BinaryFragment when
      SymRefs :: map(),
      BinaryFragment :: binfrag(rhash()).

decode_hash(<<Size:8/integer, Rest/binary>>, SymRefs) ->
    decode_hash(Size - ?OFFSET, Rest, maps:new(), SymRefs).

-spec decode_hash(Size, <<_:8, _:_*8>>, Decoded, SymRefs) -> BinaryFragment when
      Size :: pos_integer(),
      Decoded :: rhash(),
      SymRefs :: map(),
      BinaryFragment :: binfrag(rhash()).

decode_hash(Size, Bin, Decoded, SymRefs) when Size =< 0 ->
    {Decoded, Bin, SymRefs};
decode_hash(_Size, <<>>, _Decoded, SymRefs) ->
    {#{}, <<>>, SymRefs};
decode_hash(Size, <<Type:8/integer, Rest/binary>>, Decoded, SymRefs) ->
    {DecodedKey, Bin, NewSymRefs} = decode_chunk(Type, Rest, SymRefs),
    <<Type2:8/integer, Rest2/binary>> = Bin,
    {DecodedVal, Bin2, NewSymRefs2} = decode_chunk(Type2, Rest2, NewSymRefs),
    decode_hash(Size - 1, Bin2, maps:put(DecodedKey, DecodedVal, Decoded), NewSymRefs2).
