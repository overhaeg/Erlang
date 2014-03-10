%% terminal_reader reads data from the terminal (has to be configured properly
%% with -icanon), and sends corresponding events to the receiver (using the
%% snake protocol).
-module(terminal_reader).

%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

% Start a reader, that will send its events to the given receiver.
start(InputReceiver) ->
    processInput(InputReceiver, "").

%%
%% Local Functions
%%
processInput(InputReceiver, Data) ->
    % Clear message buffer
    receive
        after 0 ->
            null
    end,
    CharRead = io:get_chars('', 1),
    NewData = lists:append(Data, CharRead),
    case NewData of
        % 27 = ESC: should be followed by two bytes, if it isn't, wait on them
        [27] ->
            processInput(InputReceiver, NewData);
        [27, _] ->
            processInput(InputReceiver, NewData);
        "[A" -> % = [27, '[', 'A'] = ESC [ A = up arrow key
            snake:move_up(InputReceiver);
        "[B" ->
            snake:move_down(InputReceiver);
        "[C" ->
            snake:move_right(InputReceiver);
        "[D" ->
            snake:move_left(InputReceiver);
        "q" ->
            InputReceiver ! quit;
        _ -> % unknown character, ignore
            nothing
    end,
    processInput(InputReceiver, "").

