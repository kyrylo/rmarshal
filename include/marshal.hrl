%% Marshal format: https://github.com/ruby/ruby/blob/trunk/doc/marshal.rdoc

-define(MARSHAL_MAJOR, 4).
-define(MARSHAL_MINOR, 8).

-define(TYPE_NIL,        $0).
-define(TYPE_TRUE,       $T).
-define(TYPE_FALSE,      $F).

%% A signed 32 bit value.
-define(TYPE_FIXNUM,     $i).

-define(TYPE_EXTENDED,   $e).
-define(TYPE_UCLASS,     $C).
-define(TYPE_OBJECT,     $o).
-define(TYPE_DATA,       $d).
-define(TYPE_USERDEF,    $u).
-define(TYPE_USRMARSHAL, $U).
-define(TYPE_FLOAT,      $f).
-define(TYPE_BIGNUM,     $l).
-define(TYPE_STRING,     $").
-define(TYPE_REGEXP,     $/).
-define(TYPE_ARRAY,      $[).
-define(TYPE_HASH,       ${).
-define(TYPE_HASH_DEF,   $}).
-define(TYPE_STRUCT,     $S).
-define(TYPE_MODULE_OLD, $M).
-define(TYPE_CLASS,      $c).
-define(TYPE_MODULE,     $m).

-define(TYPE_SYMBOL,     $:).
-define(TYPE_SYMLINK,    $;).

-define(TYPE_IVAR,       $I).
-define(TYPE_LINK,       $@).

-define(OFFSET, 5).
-define(CHAR, trunc(math:pow(2, 8))).
-define(INT16, trunc(math:pow(2, 16))).
-define(INT24, trunc(math:pow(2, 24))).
-define(INT32, trunc(math:pow(2, 32))).

-define(BIGNUM_LEN(Len), Len * 2 - 10).

-type rnil() :: nil.
-type rbool() :: true | false.
-type fixnum() :: integer().
-type bignum() :: integer().
-type rfloat() :: float().
-type rhash() :: map().
-type rsymbol() :: atom().
-type rstring() :: list().
-type rarray() :: [rterm()].

-type rterm() :: rnil()
               | rbool()
               | fixnum()
               | bignum()
               | rfloat()
               | rhash()
               | rsymbol()
               | rstring()
               | rarray().

-type binfrag(RTermType) :: {RTermType, Undecoded :: binary(), map()}.
