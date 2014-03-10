-module(control).

-include_lib("eunit/include/eunit.hrl").

%%
%% Local Functions
%%

%% Case

%% Fix the case statement to satisfy the unit test!
case_statement(X, Y) ->
    case {Y, X} of
        {1, do} -> do1;
        {2, do} -> do2;
        {_Ignored, helloWorld} -> helloUser;
        Else -> process_somehow(Else)
    end.

process_somehow(_Value) -> implemented.

case_statement_test() ->
    ?assertMatch(do1,         case_statement(do, 1)),
    ?assertMatch(implemented, case_statement(something, 2)),
    ?assertEqual(helloUser,   case_statement(helloWorld, 3)).

%% If


if_statement(Value) ->
    if
		is_atom(Value) -> RndValue = Value, OrgValue = Value;
		true -> {RndValue, OrgValue} = Value
	end,
    if
        RndValue == 3 -> three;
        RndValue >= 4, is_number(RndValue) -> fourOrMore;
        is_list(OrgValue) -> bar;
        true -> thisIsTheElseBranch
    end.

if_statement_test() ->
    ?assertMatch(three,               if_statement({3, foo})),
    ?assertMatch(fourOrMore,          if_statement({4, foo})),
    ?assertMatch(bar,                 if_statement({x, [foo]})), % what happens here? | x != 3 and is not a number 
																 % -> 2 first branches fail. Because OrgValue ([foo]) is a list, the third branch will succeed and return bar.
    ?assertMatch(thisIsTheElseBranch, if_statement(someAtom)). % something is going wrong, how do you prevent this situation? add a test to see if an atom was given to if_statement.

%% Exceptions

try_something(Value) ->
    Var = this_wont_work(Value),
    case Var of
        ok -> true;
        _Else -> someError
    end.

this_wont_work(Value) ->
    {test, foo, Var} = Value,
    Var.

exceptions_test() ->
    case catch try_something(foo) of
        foo ->
            s;
        _Else ->
            true % erlang:display(Else)
    end.

