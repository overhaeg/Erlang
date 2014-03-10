-module(hello_world).

%% Exported Functions
-export([start/0]).

%% External functions
start() ->
    io:fwrite("Hello World!~n").
