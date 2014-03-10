%% Constants and types used by the program.

-define(WIDTH, 10).
-define(HEIGHT, 10).
-define(APPLE_CNT, 5).

% Board record type
-record(board, {height=1, width=1, board=[[free]]}).

