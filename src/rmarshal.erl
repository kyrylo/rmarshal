-module(rmarshal).
-export([load/1]).

-include("marshal.hrl").

-spec load(<<_:16, _:_*8>>) -> {'ok', rterm()}.

load(<<?MARSHAL_MAJOR:8, ?MARSHAL_MINOR:8, Rest/binary>>) ->
    rmarshal_symstorage:start(),
    decode(Rest, []).

-spec decode(<<_:8, _:_*8>>, Decoded) -> {Undecoded, Decoded} when
      Decoded :: rarray(),
      Undecoded :: binary();
            (<<>>, Decoded) -> {'ok', Decoded} when
      Decoded :: rarray().

decode(<<>>, Decoded) ->
    rmarshal_symstorage:stop(),
    {ok, Decoded};
decode(<<Type:8, Rest/binary>>, Decoded) ->
    {DecodedChunk, UndecodedRest} = decode_chunk(Type, Rest),
    decode(UndecodedRest, [DecodedChunk|Decoded]).

-spec decode_chunk(Type, Undecoded) -> BinaryFragment when
      Type :: byte(),
      Undecoded :: binary(),
      BinaryFragment :: binfrag(rterm()).

decode_chunk(?TYPE_NIL,     Bin) -> {nil, Bin};
decode_chunk(?TYPE_TRUE,    Bin) -> {true, Bin};
decode_chunk(?TYPE_FALSE,   Bin) -> {false, Bin};
decode_chunk(?TYPE_FIXNUM,  Bin) -> decode_fixnum(Bin);
decode_chunk(?TYPE_BIGNUM,  Bin) -> decode_bignum(Bin);
decode_chunk(?TYPE_IVAR,    Bin) -> decode_ivar(Bin);
decode_chunk(?TYPE_ARRAY,   Bin) -> decode_array(Bin);
decode_chunk(?TYPE_SYMBOL,  Bin) -> decode_symbol(Bin);
decode_chunk(?TYPE_SYMLINK, Bin) -> decode_symlink(Bin);
decode_chunk(?TYPE_FLOAT,   Bin) -> decode_float(Bin);
decode_chunk(?TYPE_HASH,    Bin) -> decode_hash(Bin).

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
    case Len =:= 0 of
        true ->
            Bitstring = <<>>,
            Encoding = Rest;
        false ->
            {Bitstring, Encoding} = split_binary(Rest, Len - ?OFFSET)
    end,
    decode_string(Bitstring, Encoding).

-spec decode_string(Bitstring, <<_:40, _:_*8>>) -> BinaryFragment when
      Bitstring :: binary(),
      BinaryFragment :: binfrag(rstring()).

%% Bitstring + Encoding. I don't know how to interpret encoding.
%% WTF is T or :/;?
decode_string(Bitstring, <<6, $:, 6, $E, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest};
decode_string(Bitstring, <<6, $;, 0, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest}.

-spec decode_array(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rterm()).

decode_array(<<Size:8, Rest/binary>>) ->
    decode_array(Size - ?OFFSET, Rest, []).

-spec decode_array(Size, <<_:8, _:_*8>>, Decoded) -> BinaryFragment when
      Size :: pos_integer(),
      Decoded :: rarray(),
      BinaryFragment :: binfrag(rterm()).

decode_array(0, Bin, Decoded) ->
    {lists:reverse(Decoded), Bin};
decode_array(_Size, <<>>, _Decoded) ->
    {[], <<>>};
decode_array(Size, <<?TYPE_ARRAY, 0, Rest/binary>>, Decoded) ->
    decode_array(Size - 1, Rest, [[]|Decoded]);
decode_array(Size, <<Type:8/integer, Rest/binary>>, Decoded) ->
    {DecodedChunk, Bin} = decode_chunk(Type, Rest),
    decode_array(Size - 1, Bin, [DecodedChunk|Decoded]).

-spec decode_symbol(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rsymbol()).

decode_symbol(<<Len:8/integer, Rest/binary>>) ->
    {Sym, Bin} = split_binary(Rest, Len - ?OFFSET),
    Atom = list_to_atom(binary_to_list(Sym)),
    rmarshal_symstorage:write(Atom),
    {Atom, Bin}.

-spec decode_symlink(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rsymbol()).

decode_symlink(<<SymLink:8/integer, Rest/binary>>) ->
    {ok, Atom} = rmarshal_symstorage:read(SymLink),
    {Atom, Rest}.

-spec decode_float(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rfloat()).

decode_float(<<Len:8/integer, Rest/binary>>) ->
    {Float, Bin} = split_binary(Rest, Len - ?OFFSET),
    {binary_to_float(Float), Bin}.

-spec decode_hash(<<_:8, _:_*8>>) -> BinaryFragment when
      BinaryFragment :: binfrag(rhash()).

decode_hash(<<Size:8/integer, Rest/binary>>) ->
    decode_hash(Size - ?OFFSET, Rest, maps:new()).

-spec decode_hash(Size, <<_:8, _:_*8>>, Decoded) -> BinaryFragment when
      Size :: pos_integer(),
      Decoded :: rhash(),
      BinaryFragment :: binfrag(rhash()).

decode_hash(Size, Bin, Decoded) when Size =< 0 ->
    {Decoded, Bin};
decode_hash(_Size, <<>>, _Decoded) ->
    {#{}, <<>>};
decode_hash(Size, <<Type:8/integer, Rest/binary>>, Decoded) ->
    {DecodedKey, Bin} = decode_chunk(Type, Rest),
    <<Type2:8/integer, Rest2/binary>> = Bin,
    {DecodedVal, Bin2} = decode_chunk(Type2, Rest2),
    decode_hash(Size - 1, Bin2, maps:put(DecodedKey, DecodedVal, Decoded)).
