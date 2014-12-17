-module(rmarshal).
-export([load/1]).

-include("marshal.hrl").

load(<<?MARSHAL_MAJOR:8, ?MARSHAL_MINOR:8, Rest/binary>>) ->
    rmarshal_symstorage:start(),
    decode(Rest, []).

decode(<<>>, Decoded) ->
    rmarshal_symstorage:stop(),
    {ok, Decoded};
decode(<<Type:8, Rest/binary>>, Decoded) ->
    {DecodedChunk, UndecodedRest} = decode_chunk(Type, Rest),
    decode(UndecodedRest, [DecodedChunk|Decoded]).

decode_chunk(?TYPE_NIL,     Bin) -> {nil, Bin};
decode_chunk(?TYPE_TRUE,    Bin) -> {true, Bin};
decode_chunk(?TYPE_FALSE,   Bin) -> {false, Bin};
decode_chunk(?TYPE_FIXNUM,  Bin) -> decode_fixnum(Bin);
decode_chunk(?TYPE_BIGNUM,  Bin) -> decode_bignum(Bin);
decode_chunk(?TYPE_IVAR,    Bin) -> decode_ivar(Bin);
decode_chunk(?TYPE_ARRAY,   Bin) -> decode_array(Bin);
decode_chunk(?TYPE_SYMBOL,  Bin) -> decode_symbol(Bin);
decode_chunk(?TYPE_SYMLINK, Bin) -> decode_symlink(Bin);
decode_chunk(?TYPE_FLOAT,   Bin) -> decode_float(Bin).

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

decode_bignum(<<Sign:8/bitstring, Len:8, Rest/binary>>) ->
    {X, Bin} = split_binary(Rest, ?BIGNUM_LEN(Len)),
    Decoded = binary:decode_unsigned(X, little),
    case Sign of
        <<$+>> -> {Decoded, Bin};
        <<$->> -> {Decoded * -1, Bin}
    end.

decode_ivar(<<?TYPE_STRING, Rest/binary>>) ->
    decode_string(Rest).

decode_string(<<Len:8/integer, Rest/binary>>) ->
    case Len =:= 0 of
        true ->
            Bitstring = <<>>,
            Encoding = Rest;
        false ->
            {Bitstring, Encoding} = split_binary(Rest, Len - ?OFFSET)
    end,
    decode_string(Bitstring, Encoding).

%% Bitstring + Encoding. I don't know how to interpret encoding.
%% WTF is T or :/;?
decode_string(Bitstring, <<6, $:, 6, $E, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest};
decode_string(Bitstring, <<6, $;, 0, $T, Rest/binary>>) ->
    {binary_to_list(Bitstring), Rest}.

decode_array(<<Size:8, Rest/binary>>) ->
    decode_array(Size - ?OFFSET, Rest, []).

decode_array(0, Bin, Decoded) ->
    {lists:reverse(Decoded), Bin};
decode_array(_Size, <<>>, _Decoded) ->
    {[], <<>>};
decode_array(Size, <<?TYPE_ARRAY, 0, Rest/binary>>, Decoded) ->
    decode_array(Size - 1, Rest, [[]|Decoded]);
decode_array(Size, <<Type:8/integer, Rest/binary>>, Decoded) ->
    {DecodedChunk, Bin} = decode_chunk(Type, Rest),
    decode_array(Size - 1, Bin, [DecodedChunk|Decoded]).

decode_symbol(<<Len:8/integer, Rest/binary>>) ->
    {Sym, Bin} = split_binary(Rest, Len - ?OFFSET),
    Atom = list_to_atom(binary_to_list(Sym)),
    rmarshal_symstorage:write(Atom),
    {Atom, Bin}.

decode_symlink(<<SymLink:8/integer, Rest/binary>>) ->
    {ok, Atom} = rmarshal_symstorage:read(SymLink),
    {Atom, Rest}.

decode_float(<<Len:8/integer, Rest/binary>>) ->
    {Float, Bin} = split_binary(Rest, Len - ?OFFSET),
    {binary_to_float(Float), Bin}.
