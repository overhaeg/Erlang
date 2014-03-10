-module(manager).

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
    Work = generated_work(),

    % start workers to process the work, use unreliable_worker:work/1
    Workers = lists:map(fun spawner/1, Work),
    wait_on_workers(Workers).

spawner(Work) ->
	spawn_link(unreliable_worker, work, [Work]).	
	
%%
%% Local Functions
%%
generated_work() ->
	[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p].

wait_on_workers([]) ->
    io:fwrite("All Work Done~n");
wait_on_workers(Tasks) ->
    receive
        {'EXIT', Pid, Reason} ->
            io:fwrite("Pid ~w exited with reason: ~w~n", [Pid, Reason]),
			case Reason of
				normal -> wait_on_workers(lists:delete(Pid,Tasks));
				{{nocatch, {unreliable_worker, task_unfinished,W}},_} ->
					NewWorker = spawner(W),
					NewL = lists:delete(Pid,Tasks),
					wait_on_workers(lists:append(NewL,[NewWorker]))
			end;
		SomethingElse ->
            io:fwrite("Something else: ~w~n", [SomethingElse]),
			wait_on_workers(Tasks)
			
    end.

