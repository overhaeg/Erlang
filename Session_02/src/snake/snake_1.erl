%% snake is the protocol of the snake, used by terminal_reader (for human
%% player) or snake_ai (for AI player) to communicate with game_master.
-module(snake).

%%
%% Exported Functions
%%
-export([move_left/1,
         move_right/1,
         move_up/1,
         move_down/1,
         move_direction/2,
         return_data/2]).

%%
%% API Functions
%%

%% Input for steering
move_left(Receiver) ->
    Receiver ! {self(), left}.

move_right(Receiver) ->
    Receiver ! {self(), right}.

move_up(Receiver) ->
    Receiver ! {self(), up}.

move_down(Receiver) ->
    Receiver ! {self(), down}.

move_direction(Receiver, Direction) ->
    Receiver ! {self(), Direction}.

%% Output for next steering step
return_data(Receiver, {Board, Snake, Direction}) ->
    Receiver ! {self(), Board, Snake, Direction}.

