%% terminal provides low level access the common ANSI terminal. It provides
%% functions to clean, move and write terminal output.
-module(terminal).

%%
%% Exported Functions
%%
-export([clean/0,
         set_cursor/2,
         move_forward/1,
         put/1]).

%%
%% API Functions
%%
clean() ->
    io:fwrite("[2J").

set_cursor(X, Y) ->
    io:fwrite("[~w;~wH", [Y, X]).

move_forward(X) ->
    if
        X /= 0 -> io:fwrite("[~wC", [X]);
        true   -> nothing
    end.

put(String) ->
    io:fwrite(String).

