%% board_view draws the board on the screen. It provides an API to game_master
%% to draw the board, snakes and apples.
-module(board_view).

%%
%% Include files
%%
-include("definitions.hrl").

%%
%% Exported Functions
%%
-export([start/0,
         init/0,
         display_board/1,
         show_snake_head/1,
         show_apple/1,
         free/1,
         debug/2]).

%%
%% API Functions
%%

% Initialize this component
init() ->
    register(board_view, self()).

% Start board_view. Runs until game is over.
start() ->
    terminal:clean(),
    eventLoop(dict:new()).

% Update (initialize) complete board
display_board(Board) ->
    board_view ! {board, Board, self()}.

% Mark cell as containing snake
show_snake_head(Pos) ->
    board_view ! {snake, Pos, self()}.

% Mark cell as containing apple
show_apple(Pos) ->
    board_view ! {apple, Pos, self()}.

% Mark cell as free
free(Pos) ->
    board_view ! {free, Pos, self()}.

% Write debug message
debug(Pos, Message) ->
    board_view ! {debug, Pos, self(), Message}.

%%
%% Local Functions
%%
eventLoop(Dict) ->
    receive
        {board, Board, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            update_completely(Board, Offset);
        {snake, {X, Y}, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            put_field(snake, X, Y, Offset);
        {apple, {X, Y}, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            put_field(apple, X, Y, Offset);
        {free, {X, Y}, Sender} ->
            {NewDict, Offset} = updateRegisteredViews(Sender, Dict),
            clean_field(X, Y, Offset);
        {debug, {X, Y}, Sender, Message} ->
            NewDict = Dict,
            terminal:set_cursor(X + 1, Y + 1), % +1 because of border
            terminal:put(Message);
        SomeThing ->
            erlang:display(SomeThing),
            NewDict = Dict
    end,
    eventLoop(NewDict).

updateRegisteredViews(Sender, Dict) ->
    case dict:is_key(Sender, Dict) of
        true ->
            Offset = dict:fetch(Sender, Dict),
            {Dict, Offset};
        false ->
            Offset = newOffset(dict:size(Dict)),
            {dict:store(Sender, Offset, Dict), Offset}
    end.

newOffset(Index) ->
    Y = ((Index * 12) div (12 * 5)) * 12,
    X = (Index * 12) rem (12 * 5),
    {X, Y}.

update_completely(Board, Offset) ->
    draw_border(?HEIGHT, ?WIDTH, Offset),
    update_rows(Board#board.board, 1, 1, Offset).

clean_field(X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY), % +1 because of border
    terminal:put(' ').

put_field(apple, X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY),
    terminal:put('o');
put_field(snake, X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY),
    terminal:put('#').

draw_border(Height, Width, Offset) ->
    draw_rows(Height, Width, Offset, true).

draw_rows(0, Width, {OffX, OffY}, false) ->
    terminal:move_forward(OffX),
    terminal:put('\\'),
    draw_col(Width, '-'),
    terminal:put('/');
draw_rows(Height, Width, {OffX, OffY}, true) ->
    terminal:set_cursor(1 + OffX, 1 + OffY),
    terminal:put('/'),
    draw_col(Width, '-'),
    terminal:put('\\~n'),
    draw_rows(Height, Width, {OffX, OffY}, false);
draw_rows(Height, Width, {OffX, OffY}, false) ->
    terminal:move_forward(OffX),
    terminal:put('|'),
    draw_col(Width, ' '),
    terminal:put('|~n'),
    draw_rows(Height - 1, Width, {OffX, OffY}, false).

draw_col(0, _) ->
    null;
draw_col(Width, Char) ->
    terminal:put(Char),
    draw_col(Width - 1, Char).

update_rows([Head|Tail], X, Y, Offset) ->
    update_items(Head, X, Y, Offset),
    update_rows(Tail, X, Y + 1, Offset);
update_rows([], _, _, _) ->
    null.

update_items([apple|Tail], X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY),
    terminal:put('o'),
    update_items(Tail, X + 1, Y, {OffX, OffY});
update_items([snake|Tail], X, Y, {OffX, OffY}) ->
    terminal:set_cursor(X + 1 + OffX, Y + 1 + OffY),
    terminal:put('#'),
    update_items(Tail, X + 1, Y, {OffX, OffY});
update_items([_|Tail], X, Y, {OffX, OffY}) ->
    update_items(Tail, X + 1, Y, {OffX, OffY});
update_items([], _, _, _) ->
    null.

