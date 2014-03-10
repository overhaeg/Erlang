-module(unreliable_worker).

%%
%% Exported Functions
%%
-export([work/1]).

%%
%% API Functions
%%
work(Task) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    timer:sleep(random:uniform(500)),
    case random:uniform(4) of
        1 -> % Chance of 1/4 that it throws an error
            throw({unreliable_worker, task_unfinished, Task});
        2 -> % Chance of 1/4 that it finishes
            finished;
        _ -> % Else, try again
            work(Task)
    end.

