%% main initializes the Snake application and spawns all processes.
%% The following actors are started:
%% * game_master: executes the actual game, and enforces game rules
%% * terminal_reader: reads user input
%% * snake_ai (3x): AI players
-module(main).

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

% Start the program.
start() ->
    board_view:init(),

    start(human),
    start(ai),
    start(ai),
    start(ai),
    % Idea: try to add more AI players

    board_view:start().

%%
%% Local Functions
%%

% Start a game, either as a human or an AI player.
% Creates a new game_master, with as player either terminal_reader (for human)
% or snake_ai (for AI).
start(human) ->
    GameMaster = spawn_link(game_master, start, []),
    Id = spawn_link(terminal_reader, start, [GameMaster]),
    game_master:set_player(GameMaster, Id);
start(ai) ->
    GameMaster = spawn_link(game_master, start, []),
    Id = spawn_link(snake_ai, start, [GameMaster]),
    game_master:set_player(GameMaster, Id).

