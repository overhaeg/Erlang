-module(debugging).

%% Exported Functions
-export([start/0]).

%% External functions
start() ->
    Foo = 1 + 4,
    Bar = {Foo, bar},
    Foobar = {Foo, Bar},
    io:fwrite("Hello World!~n Bar:~w~n", [Bar]).
