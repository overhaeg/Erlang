-module(server_multiple_actors).

-export([initialize/0,
         register_user/0]).

initialize() ->
    register(manager_actor, spawn_link(?MODULE, manager_actor, [[]])),
    ok.




manager_actor(Users) ->
    receive
        
        {Sender, register_user} ->
            {NewUsers, NewUserID, NewUserActor} = add_new_user(Users),
            Sender ! {registered_user, NewUserID, NewUserActor},
            manager_actor(NewUsers);
       
        {Sender, get_data_actor, UserID} ->
            Sender ! {data_actor, get_data_actor(Users)},
            manager_actor(Users)

        end.


add_new_user(Users) ->
    NewUserId = length(Data),
    NewDataActor = spawn_link(?MODULE, data_actor, [{data, [], sets:new()}]),
    NewUsers = Users ++ [{user, NewUserId, NewDataActor}],
    NewUserActor = spawn_link(?MODULE, entry_actor, []),
    {NewData, NewUserId, NewUserActor}.



