%% board provides functions to read and modify the board.
-module(board).

%%
%% Include files
%%
-include("definitions.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([create/2,
         atPosition/2,
         overflow/2,
         changeField/3,
         randomPos/1]).

%%
%% API Functions
%%

% Create board of given size.
create(Width, Height) ->
    Row = lists:duplicate(Width, free),
    BoardData = lists:duplicate(Height, Row),
    #board{height=Height, width=Width, board=BoardData}.

% Get contents of field (X, Y): apple, snake or free.
atPosition({X, Y}, Board) ->
    Row = lists:nth(Y, Board#board.board),
    lists:nth(X, Row).

% Returns {X % board width, Y % board height}.
overflow({X, Y}, Board) ->
    {mod(X - 1, Board#board.width) + 1, mod(Y - 1, Board#board.height) + 1}.

% Update field (X, Y) in Board to NewValue, returns updated board.
changeField(Board, {X, Y}, NewValue) ->
    {Rows, RowsBelow} = lists:split(Y, Board#board.board),
    {RowsAbove, [Row]} = lists:split(Y - 1, Rows),
    {Fields, FieldsRight} = lists:split(X, Row),
    {FieldsLeft, [_OldField]} = lists:split(X - 1, Fields),
    Board#board{board=RowsAbove ++ [FieldsLeft ++ [NewValue] ++ FieldsRight] ++ RowsBelow}.

% Returns random position {X, Y} on Board.
randomPos(Board) ->
    X = random:uniform(Board#board.width),
    Y = random:uniform(Board#board.height),
    {X, Y}.

%%
%% Local Functions
%%

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _Y) -> 0.

%%
%% Unit Tests
%%

create_test() ->
    #board{height=3, width=3,
           board=[[free, free, free],
                  [free, free, free],
                  [free, free, free]]} = create(3, 3).

atPosition_test() ->
    free = atPosition({2, 1}, #board{height=2, width=2, board=
                                         [[apple, free],
                                          [apple, apple]]}).

mod_test() ->
    ?assertMatch(0, mod(0, 10)),
    ?assertMatch(1, mod(1, 10)),
    ?assertMatch(0, mod(10, 10)),
    ?assertMatch(1, mod(11, 10)).

overflow_test() ->
    Board = #board{height=3, width=3, board=[[free, free, free],
                                             [free, free, free],
                                             [free, free, free]]},
    ?assertMatch({1, 1}, overflow({1, 1}, Board)),
    ?assertMatch({1, 2}, overflow({1, 2}, Board)),
    ?assertMatch({1, 3}, overflow({1, 3}, Board)),
    ?assertMatch({1, 1}, overflow({1, 4}, Board)),
    ?assertMatch({1, 1}, overflow({4, 4}, Board)),
    ?assertMatch({3, 3}, overflow({0, 0}, Board)),
    ?assertMatch({3, 2}, overflow({0, -1}, Board)).

changeField_test() ->
    Board = #board{height=3, width=3, board=[[free, free, free],
                                             [free, free, free],
                                             [free, free, free]]},
    #board{height=3, width=3, board=[[free, free, free],
                                     [free, free, free],
                                     [apple, free, free]]}
            = changeField(Board, {1, 3}, apple).

