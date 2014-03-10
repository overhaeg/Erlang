%% Examples for the pattern matching in Erlang
-module(pattern_matching).

%% Including the following generates a test/0 function,
%% which runs all Tests and Test Generators defined in the module.
-include_lib("eunit/include/eunit.hrl").

%%
%% Local Functions
%%
simple_match(1) ->
    SomeVariableX = 42 + 8,
    AnotherVarY = someAtom,
    ATuple = {SomeVariableX, AnotherVarY};

simple_match(2) ->
    AReturnValue = simple_match(1),
    {returnValue, AReturnValue};

simple_match(extractDataFromTuples) ->
    {returnValue, {X, Y}} = simple_match(2),
    % reassemble data in some way
    {X * X, Y};

simple_match(returnAtom) ->
    {X, Y} = simple_match(extractDataFromTuples),
    Y.

simple_match_test_() -> [
	?_assert({50, someAtom} == simple_match(1)),
    ?_assert({returnValue, simple_match(1)} == simple_match(2)),
    ?_assert({2500, someAtom} == simple_match(extractDataFromTuples)),
    ?_assert(someAtom == simple_match(returnAtom))
].
