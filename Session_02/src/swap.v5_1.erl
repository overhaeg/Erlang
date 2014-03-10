-module(swap).

%%
%% Exported Functions
%%
-export([start/0, loop/1]).

%%
%% API Functions
%%
start() ->
    io:fwrite("Test code swap and speak with me~n"),
    loop([]).


loop(PreviousInput) ->
    CurrentVersion = 5,  % this is version 5
    Input = io:get_line(">> "),
    case Input of
        "reload\n" ->
            catch erlang:purge_module(swap);
            % load "swap.erl" here, using the module "code"
        _ ->
            io:fwrite("v~B: ~s", [CurrentVersion, Input]),
            io:fwrite("Old Input: ~w~n", [PreviousInput])
    end,
    loop(PreviousInput ++ [{Input, CurrentVersion}]).

