-module(pm_lists).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Local Functions
%%

% Part 1
%--------

produce_list() ->
    lists:seq(1, 10).

process_list([Head , Element2 , Element3 | Tail]) ->
    process_list(Tail);
process_list([Head | Tail]) ->
    process_list(Head);
process_list(LastElement) ->
    LastElement = 10.

% nth(N, List) -> Elem
nth(1, [Head | Tail]) ->
    Head;
nth(N, [Head | Tail]) ->
    nth(N - 1, Tail).


% Part 2
%--------

produce_list2() ->
    [a, b, a, b, a, a, a, {b}, [a, a, a]].

% Order of clauses matters!
match_list([a, b, [a, a, a]], CallNr) -> % this clause needs to be before the next one
    erlang:display("third"),
    CallNr;
match_list([a, b | Tail], CallNr) ->
    erlang:display("first"),
    match_list(Tail, CallNr + 1);
match_list([a | [A, a, {B}, List]], CallNr) ->
    erlang:display("second"),
    match_list([A, B, List], CallNr + 1).

%%
%% Test Cases
%%

nth_test_() -> [
    ?_assertMatch(5, nth(1, [5])),
    ?_assertMatch(5, nth(1, [5, 6])),
    ?_assertMatch(6, nth(2, [5, 6])),
    ?_assertMatch(5, nth(1, lists:seq(5, 9))),
    ?_assertMatch(6, nth(2, lists:seq(5, 9))),
    ?_assertMatch(7, nth(3, lists:seq(5, 9))),
    ?_assertMatch(8, nth(4, lists:seq(5, 9))),
    ?_assertMatch(9, nth(5, lists:seq(5, 9)))
].

% These tests are correct, do not change them!
listoperations_test_() -> [
    ?_assertMatch([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], produce_list()),
    ?_assertMatch(10, process_list(produce_list())),
    ?_assertMatch( 4, match_list(produce_list2(), 1))
].

