%%%-------------------------------------------------------------------
%%% @author Christophe De Troyer
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Helper functions for the Erlang project
%%%
%%% @end
%%% Created : 14. mrt 2014 12:52
%%%-------------------------------------------------------------------
-module(helpers).
-author("Christophe De Troyer").

-compile([debug_info]).
-export([ % Public
          take/2, drop/2, createRandomList/3, shuffle/1, sortAccordingToLoad/1, sortTweets/1]).


%**************************************************************************************************
% Take N elements from the list
take(_N, []) ->
  [];
take(0, _List) ->
  [];
take(N, [H | T]) ->
  [H | take(N - 1, T)].

% Drop N elements from a list
drop(_, []) -> [];
drop(0, List) -> List;
drop(N, [_H | Rest]) ->
  drop(N -1, Rest).

%**************************************************************************************************
% Creating a random list

randBetween(Minimum, Maximum) ->
  Upper = Maximum - Minimum,
  random:uniform(Upper + 1) + Minimum - 1.

% Generate a random list of integers between bounds
createRandomList(Min, Max, Length) ->
  createRandomList(Min, Max, [], Length).

createRandomList(_, _, Acc, 0) -> Acc;
createRandomList(Min, Max, Acc, Length) ->
  createRandomList(Min, Max, [randBetween(Min, Max) | Acc], Length - 1).

%**************************************************************************************************
% Shuffle a list (Fisher-Yates)

shuffle([]) ->
    [];
shuffle([E]) ->
    [E];
shuffle(List) ->
    G = round(random:uniform() * (length(List) - 1)) + 1,   % 1-based indexing of elements
    [N, N2 |R] = swap_elements(1, G, List),
    [N, N2 |shuffle(R)].  % not tail recursive
% Swap element on position A with B
swap_elements(Ida, Idb, List) when Ida == Idb ->
    List;
swap_elements(Ida, Idb, List) ->
    Z = lists:zip(lists:seq(1, length(List)), List),
    {_, A} = lists:keysearch(Ida, 1, Z),
    {_, B} = lists:keysearch(Idb, 1, Z),
    {_, Result} = lists:unzip(lists:keyreplace(Ida, 1, lists:keyreplace(Idb,1, Z, A) , B)),
    Result.

%**************************************************************************************************
% Sorting Functions

sortAccordingToLoad(List) ->
  Comparer = fun({_Actor1, X}, {_Actor2, Y}) ->
              X < Y
             end,
  quicksort(List, Comparer).

sortTweets(List) ->
  Comparer = fun({tweet, _, Ts1, _}, {tweet, _, Ts2, _})  ->
                Ts1 < Ts2
             end,
  quicksort(List, Comparer). 

quicksort([Pivot|T], Comparer) ->
    quicksort([ X || X <- T, Comparer(X, Pivot)], Comparer) ++
    [Pivot] ++
    quicksort([ X || X <- T, not(Comparer(X, Pivot))], Comparer);
quicksort([],_) -> [].