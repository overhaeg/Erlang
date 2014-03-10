-module(records).

-include_lib("eunit/include/eunit.hrl").

%%
%% Records can be also specified in header files (*.hrl)
%%
-record(customer, {name=null, addresses=[], phone={}}).
-record(address, {street=null, city=null, zip=null, state=null}).

%%
%% Local Functions
%%

retrieve_customers() ->
    % we are not going to use a real DB here, but just return some test data
    [
     #customer{name="Hans Meyer",
               phone={be, 0333, 234232}},
     #customer{name="George Smith",
               addresses=[#address{street="Banana Street 4", city="Test"}]}
     ].

records_are_just_syntax_sugar_test() ->
    [Cust1, _Cust2] = retrieve_customers(),
    ?assertMatch({customer, "Hans Meyer", [], {be, 0333, 234232}}, Cust1),
    {customer, Name1, _, _} = Cust1,
    Name2 = Cust1#customer.name,
    ?assertMatch(Name1, Name2),
    ?assertMatch("Hans Meyer", Name1).

% Fix the address, add a city to the record definition and make sure the test case do not fail anymore
address_record_test() ->
    [_Cust1, Cust2] = retrieve_customers(),
    [Addr] = Cust2#customer.addresses,
    ?assert(erlang:is_list(Addr#address.street)),
    ?assert(erlang:is_list(Addr#address.city)),  % <-- uncomment this line
    nothing. % could be removed, just added to make the compiler happy

problems_without_syntax_sugar_test() ->
    HardCodedAddress = {address, "Banana Street 4", "Tualu", 34683, "Florida"},
    Zip1 = HardCodedAddress#address.zip,
    {address, _Street, _City, Zip2, _State} = HardCodedAddress,
    ?assertMatch(Zip1, Zip2).

