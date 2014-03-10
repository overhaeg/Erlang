-module(my_queue).
-include_lib("eunit/include/eunit.hrl").

-export([queue_empty/0]).

%%
%% Client API
%%
create_queue() ->
    spawn(?MODULE, queue_empty, []).

is_empty(QPid) ->
    QPid ! {self(), is_empty},
    receive
        {is_empty, Result} -> Result
    end.

enqueue(QPid, Value) ->
    QPid ! {self(), enqueue, Value}.

dequeue(QPid) ->
    QPid ! {self(), dequeue},
    receive
        {result, Value} -> Value
    end.

%% 
%% Queue
%%

% Implement your queue here, starting with queue_empty()
 queue_full(Value) ->
	receive
		{Sender, is_empty} ->
			Sender ! {is_empty, false},
			queue_full(Value);
		{Sender, dequeue} ->
			Sender ! {result, Value},
			queue_empty()
		end.
 
 queue_empty() ->
	receive
		{Sender, is_empty} ->
			Sender ! {is_empty, true},
			queue_empty();
		{Sender, enqueue, Value} ->
			queue_full(Value)
		end.
     



%%
%% Test Functions
%%
empty_test() ->
    QPid = create_queue(),
    ?assertMatch(true, is_empty(QPid)),
    enqueue(QPid, 1),
    ?assertMatch(false, is_empty(QPid)),
    dequeue(QPid),
    ?assertMatch(true, is_empty(QPid)).

enqueue_dequeue_test() ->
    QPid = create_queue(),
    enqueue(QPid, 1),
    ?assertMatch(1, dequeue(QPid)),

    enqueue(QPid, 1),
    enqueue(QPid, 2),
    ?assertMatch(1, dequeue(QPid)),
    ?assertMatch(2, dequeue(QPid)).

