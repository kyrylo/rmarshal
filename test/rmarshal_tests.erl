-module(rmarshal_tests).
-include_lib("eunit/include/eunit.hrl").

-define(DECODE(Filename), element(2, rmarshal:load(fixture(Filename)))).

%% Extracts the first value from the decoded list. Used for comparing only 1
%% element.
-define(ASSERT_EQL(Expected, Filename),
        ?assertEqual(Expected, hd(?DECODE(Filename)))).

%% Returns a list.
-define(ASSERT_EQL_LIST(Expected, Filename),
        ?assertEqual(Expected, ?DECODE(Filename))).

fixture(FixtureName) ->
    case file:read_file("../test/fixtures/" ++ FixtureName ++ ".dat") of
        {ok, Data} -> Data;
        Any -> Any
    end.

load_nil_test() ->
    ?ASSERT_EQL(nil, "nil").

load_true_test() ->
    ?ASSERT_EQL(true, "true").

load_false_test() ->
    ?ASSERT_EQL(false, "false").

%%---------------
%% FIXNUM
%%---------------

load_integer_0_test() ->
    ?ASSERT_EQL(0, "integer_0").

load_integer_neg_122_test() ->
    ?ASSERT_EQL(-122, "integer_neg_122").

load_integer_pos_122_test() ->
    ?ASSERT_EQL(122, "integer_pos_122").

load_integer_neg_123_test() ->
    ?ASSERT_EQL(-123, "integer_neg_123").

load_integer_pos_123_test() ->
    ?ASSERT_EQL(123, "integer_pos_123").

load_integer_neg_124_test() ->
    ?ASSERT_EQL(-124, "integer_neg_124").

load_integer_pos_124_test() ->
    ?ASSERT_EQL(124, "integer_pos_124").

load_integer_neg_125_test() ->
    ?ASSERT_EQL(-125, "integer_neg_125").

load_integer_pos_125_test() ->
    ?ASSERT_EQL(125, "integer_pos_125").

load_integer_neg_200_test() ->
    ?ASSERT_EQL(-200, "integer_neg_200").

load_integer_pos_200_test() ->
    ?ASSERT_EQL(200, "integer_pos_200").

load_integer_neg_255_test() ->
    ?ASSERT_EQL(-255, "integer_neg_255").

load_integer_pos_255_test() ->
    ?ASSERT_EQL(255, "integer_pos_255").

load_integer_neg_256_test() ->
    ?ASSERT_EQL(-256, "integer_neg_256").

load_integer_pos_256_test() ->
    ?ASSERT_EQL(256, "integer_pos_256").

load_integer_neg_257_test() ->
    ?ASSERT_EQL(-257, "integer_neg_257").

load_integer_pos_257_test() ->
    ?ASSERT_EQL(257, "integer_pos_257").

load_integer_neg_33333_test() ->
    ?ASSERT_EQL(-33333, "integer_neg_33333").

load_integer_pos_33333_test() ->
    ?ASSERT_EQL(33333, "integer_pos_33333").

load_integer_neg_65535_test() ->
    ?ASSERT_EQL(-65535, "integer_neg_65535").

load_integer_pos_65535_test() ->
    ?ASSERT_EQL(65535, "integer_pos_65535").

load_integer_neg_65536_test() ->
    ?ASSERT_EQL(-65536, "integer_neg_65536").

load_integer_pos_65536_test() ->
    ?ASSERT_EQL(65536, "integer_pos_65536").

load_integer_neg_65537_test() ->
    ?ASSERT_EQL(-65537, "integer_neg_65537").

load_integer_pos_65537_test() ->
    ?ASSERT_EQL(65537, "integer_pos_65537").

load_integer_neg_1073741824_test() ->
    ?ASSERT_EQL(-1073741824, "integer_neg_1073741824").

load_integer_pos_1073741823_test() ->
    ?ASSERT_EQL(1073741823, "integer_pos_1073741823").

%%---------------
%% BIGNUM
%%---------------

load_integer_pos_1073741824_test() ->
    ?ASSERT_EQL(1073741824, "integer_pos_1073741824").

load_integer_neg_1073741825_test() ->
    ?ASSERT_EQL(-1073741825, "integer_neg_1073741825").

load_integer_neg_99999991073741825_test() ->
    ?ASSERT_EQL(-99999991073741825, "integer_neg_99999991073741825").

load_long_pos_99999991073741825_test() ->
    ?ASSERT_EQL(99999991073741825, "integer_pos_99999991073741825").

%%---------------
%% STRING
%%---------------

load_string_hello_world_test() ->
    ?ASSERT_EQL("Hello, world!", "string").

load_string_rus_hello_world_test() ->
    ?ASSERT_EQL(binary_to_list(<<"Привет, мир!"/utf8>>), "string_rus").

load_string_empty_test() ->
    ?ASSERT_EQL("", "string_empty").

load_string_one_character_test() ->
    ?ASSERT_EQL("a", "string_one_character").

%%---------------
%% ARRAY
%%---------------

load_array_integers_test() ->
    ?ASSERT_EQL_LIST([[1, 2, 3]], "array_integers").

load_array_bignums_test() ->
    ?ASSERT_EQL_LIST([[1000, 2000, 30000000]], "array_bignums").

load_array_nil_false_true_test() ->
    ?ASSERT_EQL_LIST([[nil, false, true]], "array_nil_false_true").

load_array_strings_test() ->
    ?ASSERT_EQL_LIST([["one", "two", "ERLANG!"]], "array_strings").

load_array_mixed_test() ->
    ?ASSERT_EQL_LIST([[1, "one", 123456789, false]], "array_mixed").

load_array_nested_test() ->
    ?ASSERT_EQL_LIST([[[1, 4], [2, 5], [3, 6]]], "array_nested").

load_array_empty_test() ->
    ?ASSERT_EQL_LIST([[]], "array_empty").

load_array_empty_nested_test() ->
    ?ASSERT_EQL_LIST([[[], [], [[]]]], "array_empty_nested").

load_array_empty_and_nonempty_test() ->
    ?ASSERT_EQL_LIST([[[], [1, 2], [[]]]], "array_empty_and_nonempty").

%%---------------
%% SYMBOL
%%---------------

load_symbol_hello_test() ->
    ?ASSERT_EQL(hello, "symbol_hello").

load_symbol_array_test() ->
    ?ASSERT_EQL_LIST([[hello, goodbye, hello, goodbye, hi]], "symbol_array").

%%---------------
%% FLOAT
%%---------------

load_float_pos_123_456_test() ->
    ?ASSERT_EQL(123.456, "float_pos_123.456").

load_float_neg_123_456_test() ->
    ?ASSERT_EQL(-123.456, "float_neg_123.456").

%%---------------
%% HASH
%%---------------

load_hash_simple_test() ->
    ?ASSERT_EQL(#{simple => 1, hash => 2}, "hash_simple").

load_hash_nested_test() ->
    ?ASSERT_EQL(#{simple => 1, nested => #{hi => 3}, hash => 2}, "hash_nested").

load_hash_empty_test() ->
    ?ASSERT_EQL(#{}, "hash_empty").

load_hash_empty_nested_test() ->
    ?ASSERT_EQL(#{#{} => #{}}, "hash_empty_nested").

load_hash_string_keys_test() ->
    ?ASSERT_EQL(#{"one" => 1, "two" => 2}, "hash_string_keys").

load_hash_same_key_and_val_test() ->
    ?ASSERT_EQL(#{one => one}, "hash_same_key_and_val").

load_hash_arrays_symbols_and_strings_test() ->
    ?ASSERT_EQL(
       [#{event => module_added, data => [#{foo => bar, baz => "1"}]}],
       "hash_arrays_symbols_and_strings"
      ).
