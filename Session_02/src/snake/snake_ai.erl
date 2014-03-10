%% snake_ai implements an AI player, that sends events to the receiver (using
%% the snake protocol).
%%
%% The AI is simple: if we are next to an apple, go for it; otherwise, either
%% continue forward (26/30 chance, only if cell is free) or move randomly
%% left/right/up/down (4/30 chance, only if cell is free).
-module(snake_ai).

%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

% Start a reader, that will send its events to the given receiver.
start(InputReceiver) ->
    board_view:debug({2, 15}, "S"),
    board_view:debug({2, 14}, pid_to_list(self())),
    processStatusMessages(InputReceiver, 0).

%%
%% Local Functions
%%
processStatusMessages(InputReceiver, Count) ->
    receive
        % Sender should == InputReceiver
        {Sender, Board, Snake, Direction} ->
            [{X, Y}|_Tail] = Snake, % get position of head of snake
            % Simple strategy: go for an apple if it is next to head
            % Read cells next to head
            CellLeft  = board:atPosition(board:overflow({X - 1, Y}, Board), Board),
            CellRight = board:atPosition(board:overflow({X + 1, Y}, Board), Board),
            CellUp    = board:atPosition(board:overflow({X, Y - 1}, Board), Board),
            CellDown  = board:atPosition(board:overflow({X, Y + 1}, Board), Board),
            case {CellLeft, CellRight, CellUp, CellDown} of
                {apple, _, _, _} -> snake:move_left(InputReceiver);
                {_, apple, _, _} -> snake:move_right(InputReceiver);
                {_, _, apple, _} -> snake:move_up(InputReceiver);
                {_, _, _, apple} -> snake:move_down(InputReceiver);
                _                -> do_random_stuff(CellLeft, CellRight, CellUp, CellDown, Direction, InputReceiver)
            end;
        Other -> % Flushes the message queue.
            error_logger:error_msg(
                "[SnakeAI] Error: Process ~w got unknown msg ~w~n.",
                [self(), Other])
    end,
    board_view:debug({3 + Count, 15}, "T"),
    processStatusMessages(InputReceiver, Count + 1).

do_random_stuff(CellLeft, CellRight, CellUp, CellDown, Direction, InputReceiver) ->
    case {random:uniform(30), CellLeft, CellRight, CellUp, CellDown, Direction} of
        % 4/30 chance to move direction, but only if it's free
        {1, free, _, _, _, _} -> snake:move_left(InputReceiver);
        {2, _, free, _, _, _} -> snake:move_right(InputReceiver);
        {3, _, _, free, _, _} -> snake:move_up(InputReceiver);
        {4, _, _, _, free, _} -> snake:move_down(InputReceiver);
        % continue in same direction, but only if it's free
        {_, free, _, _, _, left } -> snake:move_direction(InputReceiver, Direction);
        {_, _, free, _, _, right} -> snake:move_direction(InputReceiver, Direction);
        {_, _, _, free, _, up   } -> snake:move_direction(InputReceiver, Direction);
        {_, _, _, _, free, down } -> snake:move_direction(InputReceiver, Direction);
        % otherwise, try again (random will give new result)
        _ -> do_random_stuff(CellLeft, CellRight, CellUp, CellDown, Direction, InputReceiver)
    end.

