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

% TODO: implement nth(N, List) -> Elem
% TODO: add tests for edge cases


nth(_,[]) -> [];
nth(1,List) -> hd(List);
nth(N,List) -> nth(N-1,tl(List)).


nth_test_() -> [
	?_assertMatch(3, nth(3,produce_list())),
	?_assertMatch([], nth(2,[])),
	?_assertMatch([], nth(11,produce_list()))].

% Part 2
%--------

produce_list2() ->
    [a, b, a, b, a, a, a, {b}, [a, a, a]].

match_list([a, b, [ThreeAs]], CallNr) -> % don't let the obvious fool you
    CallNr;
match_list([a, b | Tail], CallNr) ->
    match_list(Tail, CallNr + 1);
match_list([a | [A, a, {B}, List]], CallNr) ->
    match_list([A, B, List], CallNr + 1).

%%
%% Test Cases
%%

% These tests are correct, do not change them!
listoperations_test_() -> [
    ?_assertMatch([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], produce_list()),
    ?_assertMatch(10, process_list(produce_list())),
    ?_assertMatch( 4, match_list(produce_list2(), 1))
].

