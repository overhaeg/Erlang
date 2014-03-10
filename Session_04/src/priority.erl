-module(priority).
-include_lib("eunit/include/eunit.hrl").

-export([priority/1]).

create_priority_actor() ->
    spawn(?MODULE, priority, [[]]).

priority(ReceivedMsgs) ->
    ?debugFmt("History: ~w", [ReceivedMsgs]),
    timer:sleep(100),
    receive
        {Self, high, Id} ->
			priority([Id | ReceivedMsgs])
	after 0 -> 
		receive 
			{Self, high, Id} ->
				priority([Id | ReceivedMsgs]);
			{Self, low, Id} -> 
				priority([Id | ReceivedMsgs]);
			{Sender, history} ->
				Sender ! {self(), history, ReceivedMsgs},
				priority(ReceivedMsgs)
		end
    end.

%%
%% Test Functions
%%
order_test() ->
    Pid = create_priority_actor(),
    Self = self(),

    Pid ! {Self, high, 1},
    Pid ! {Self, low,  2},
    Pid ! {Self, low,  3},
    Pid ! {Self, low,  4},
    Pid ! {Self, high, 5},
    timer:sleep(1000),
    Pid ! {Self, high, 6},
    Pid ! {Self, low,  7},
    Pid ! {Self, high, 8},
    Pid ! {Self, low,  9},

    Pid ! {Self, history},
    receive
        {Pid, history, History} ->
            ?debugFmt("History: ~w", [History]),
            ?assertMatch([9, 7, 8, 6, 4, 3, 2, 5, 1], History)
    end.

