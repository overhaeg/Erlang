%% The game_master executes the actual game, and enforces game rules. It keeps
%% the board, and calls into board_view to draw it.
%%
%% A game master is initialized using start/0, and accepts players using
%% set_player/2. Once a player has been set, the game starts (i.e. its event
%% loop starts running).
-module(game_master).

%%
%% Include files
%%
-include("definitions.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([start/0,
         set_player/2,
         quit/0]).

%%
%% API Functions
%%

% Start the game. Creates the board, waits for a set_player event, and starts
% an event loop once a player has been set.
start() ->
    Board = initBoard(?WIDTH, ?HEIGHT, ?APPLE_CNT),
    board_view:display_board(Board),
    Snake = initSnake(Board),
    Direction = up,
    receive
        {set_player, Player} ->
            eventLoop(Board, Snake, Direction, Player)
    end.

% set_player gives the game master a reference to a player: either a human
% (implemented by terminal_reader) or an AI player (implemented by snake_ai).
% Player must implement the snake protocol, as defined in snake.erl.
set_player(GameMaster, Player) ->
    GameMaster ! {set_player, Player}.

% Stop the game.
quit() ->
    halt().

%%
%% Local Functions
%%
eventLoop(Board, Snake, Direction, Player) ->
    timer:sleep(500),
    receive
        Message ->
            case receive_last(Message) of
                {_Player, left} ->
                    processStep(Board, Snake, Player, left);
                {_Player, right} ->
                    processStep(Board, Snake, Player, right);
                {_Player, up} ->
                    processStep(Board, Snake, Player, up);
                {_Player, down} ->
                    processStep(Board, Snake, Player, down);
                quit ->
                    quitGame();
                Other -> % Flushes the message queue.
                    error_logger:error_msg(
                        "[Game Master] Error: Process ~w got unknown msg ~w~n.",
                        [self(), Other]),
                        processStep(Board, Snake, Player, Direction)
           end
    after 0 ->
        processStep(Board, Snake, Player, Direction)
    end.

% Will receive all messages in the queue, and return the last one.
receive_last(Last) ->
    receive
        AnyMessage ->
            receive_last(AnyMessage)
        after 0 ->
            Last
    end.

initBoard(Width, Height, AppleCnt) ->
    TmpBoard = board:create(Width, Height),
    TmpBoardWithSnake = board:changeField(TmpBoard, {1 + Width div 2, 1 + Height div 2}, snake),
    initApples(TmpBoardWithSnake, AppleCnt).

initApples(Board, 0) ->
    Board;
initApples(Board, AppleCnt) ->
    {_, NewBoard} = initApple(Board),
    initApples(NewBoard, AppleCnt - 1).

initApple(Board) ->
    Pos = board:randomPos(Board),
    case board:atPosition(Pos, Board) of
        free ->
            NewBoard = board:changeField(Board, Pos, apple),
            {Pos, NewBoard};
        _ ->
           initApple(Board) % try somewhere else
    end.

initSnake(Board) ->
    [{1 + Board#board.width div 2, 1 + Board#board.height div 2}].

processStep(Board, Snake, Player, Direction) ->
    board_view:debug({2, 13}, pid_to_list(Player)),
    NewPos = newPosition(Snake, Direction, Board),
    case board:atPosition(NewPos, Board) of
        apple ->
            % Run into apple: extend head, don't cut tail, add a new apple
            {NewSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            NewBoard = addApple(TmpBoard),
            snake:return_data(Player, {NewBoard, NewSnake, Direction}),
            eventLoop(NewBoard, NewSnake, Direction, Player);
        snake ->
            % Run into snake (other or self): game over
            quitGame();
        free ->
            % Run into free cell: extend head, cut tail
            {TmpSnake, TmpBoard} = addNewHead(NewPos, Snake, Board),
            {NewSnake, NewBoard} = removeTail(TmpSnake, TmpBoard),
            snake:return_data(Player, {NewBoard, NewSnake, Direction}),
            eventLoop(NewBoard, NewSnake, Direction, Player)
    end.

addNewHead(Pos, Snake, Board) ->
    NewSnake = [Pos] ++ Snake,
    board_view:show_snake_head(Pos),
    {NewSnake, board:changeField(Board, Pos, snake)}.

removeTail(Snake, Board) ->
    Pos = lists:last(Snake),
    NewSnake = lists:delete(Pos, Snake),
    board_view:free(Pos),
    {NewSnake, board:changeField(Board, Pos, free)}.

addApple(Board) ->
    {Pos, NewBoard} = initApple(Board), 
    board_view:show_apple(Pos),
    NewBoard.

newPosition([{X, Y}|_], Direction, Board) ->
    case Direction of
        left  -> board:overflow({X - 1, Y}, Board);
        right -> board:overflow({X + 1, Y}, Board);
        up    -> board:overflow({X, Y - 1}, Board);
        down  -> board:overflow({X, Y + 1}, Board)
    end.

quitGame() ->
    io:fwrite("GAME OVER"),
    halt().

%%
%% Unit Tests
%%

initBoard_test() ->
    #board{height=3, width=3,
           board=[[_, _, _],
                  [_, snake, _],
                  [_, _, _]]} = initBoard(3, 3, 1).

initSnake_test() ->
    Board = #board{height=9, width=9},
    [{5,5}] = initSnake(Board).

initApple_test() ->
    Board = #board{height=3, width=3, board=[[snake, snake, snake],
                                             [snake, snake, snake],
                                             [free, snake, snake]]},
    {{1, 3}, #board{height=3, width=3, board=[[snake, snake, snake],
                                             [snake, snake, snake],
                                             [apple, snake, snake]]}}
                   = initApple(Board).

