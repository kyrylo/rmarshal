-module(rmarshal_symstorage).
-export([start/0, stop/0, write/1, read/1]).

start() ->
    register(symstorage, spawn(fun() -> loop(dict:new()) end)).

stop() ->
    {symstorage ! terminate, unregister(symstorage)}.

loop(SymDict) ->
    receive
        {From, {write, Sym}} ->
            From ! {self(), ok},
            NewSymDict = store_in_dict(dict:size(SymDict), Sym, SymDict),
            loop(NewSymDict);
        {From, {read, SymLink}} ->
            case dict:is_key(SymLink, SymDict) of
                true ->  From ! {self(), {ok, dict:fetch(SymLink, SymDict)}};
                false -> From ! {self(), not_found}
            end,
            loop(SymDict);
        terminate ->
            ok
    end.

store_in_dict(0, Sym, SymDict) ->
    dict:store(0, Sym, SymDict);
store_in_dict(Size, Sym, SymDict) ->
    dict:store(Size + 5, Sym, SymDict).

write(Atom) ->
    Pid = whereis(symstorage),
    symstorage ! {self(), {write, Atom}},
    receive
        {Pid, Msg} -> Msg
    after
        3000 ->
            timeout
    end.

read(SymLink) ->
    Pid = whereis(symstorage),
    symstorage ! {self(), {read, SymLink}},
    receive
        {Pid, Msg} -> Msg
    after
        3000 ->
            timeout
    end.
