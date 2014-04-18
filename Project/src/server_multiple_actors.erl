-module(server_multiple_actors).

-export([initialize/0,
         register_user/0,
         subscribe/3,

         manager_actor/1,
         data_actor/1,
         entry_actor/0]).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

initialize() ->
    io:format("Initialize"),
    register(manager_actor, spawn_link(?MODULE, manager_actor, [[]])),
    ok.


register_user() ->
    manager_actor ! {self(), register_user},
    receive
        {registered_user, UserId, EntryPid} -> {UserId, EntryPid}
    end.

subscribe(EntryPid, UserId, UserIdToSubscribeTo) ->
    io:format("Initial subscribe call~n"),
    EntryPid ! {self(), subscribe, UserId, UserIdToSubscribeTo},
    receive
        {EntryPid, subscribed, UserId, UserIdToSubscribeTo} -> ok
    end.

manager_actor(Users) ->
    receive
        
        {Sender, register_user} ->
            {NewUsers, NewUserID, NewUserActor} = add_new_user(Users),
            Sender ! {registered_user, NewUserID, NewUserActor},
            manager_actor(NewUsers);
       
        {Sender, get_data_actor, UserId} ->
            io:format("get_data_actor ~n"),
            Sender ! {data_actor, get_data_actor(Users, UserId)},
            manager_actor(Users)

        end.


data_actor(Data) ->
    io:format("Data Actor creation ~n"),
    receive
        {Sender, get_data} ->
            Sender ! {self(), data, Data},
            data_actor(Data);

        {Sender, follow_user, UserId, UserIdFollowing} ->
            NewData = follow_user(Data, UserIdFollowing),
            Sender ! {self(), followed_by_user, UserId, UserIdFollowing},
            data_actor(NewData);
        
        {Sender, subscribe, UserId, UserIdToSubscribeTo} ->
            io:format("subscribe ~n"),
            NewData = subscribe_to_user(Data, UserId, UserIdToSubscribeTo),
            Sender ! {self(), subscribed, UserId, UserIdToSubscribeTo},
            data_actor(NewData)
            
    end.


entry_actor() ->
    io:format("Entry actor~n"),
    ManagerActor = whereis(manager_actor),
    receive
        % RequestType ::= tweets | timeline
        {Sender, RequestType, UserId, PageOrTweetOrUserId} ->
            ManagerActor ! {self(), get_data_actor, UserId},
            io:format("After Manager Actor~n"),
            receive
                {data_actor, DataActor} ->
                    io:format("Data Actor ID"),
                    DataActor ! {self(), RequestType, UserId, PageOrTweetOrUserId},
                    receive
                        {DataActor, ResponseType, UserId, Page, Result} ->
                            Sender ! {self(), ResponseType, UserId, Page, Result};
                        {DataActor, tweet_accepted, UserId, Timestamp} ->
                            Sender ! {self(), tweet_accepted, UserId, Timestamp};
                        {DataActor, subscribed, UserId, PageOrTweetOrUserId} ->
                            Sender ! {self(), subscribed, UserId, PageOrTweetOrUserId}
                    end
            end

    end,
    entry_actor().

get_data_actor(Users, UserId) ->
    {user, UserId, DataActor} = lists:nth(UserId + 1, Users),
    DataActor.

add_new_user(Users) ->
    NewUserId = length(Users),
    NewDataActor = spawn_link(?MODULE, data_actor, [{data, [], sets:new(), sets:new()}]), %% data, tweets, subscriptions, followed by
    NewUsers = Users ++ [{user, NewUserId, NewDataActor}],
    NewUserActor = spawn_link(?MODULE, entry_actor, []),
    {NewUsers, NewUserId, NewUserActor}.

subscribe_to_user(Data, UserId, UserIdToSubscribeTo) ->
    io:format("Was Here~n"),
    {data, Tweets, Subscriptions, Followed_By} = Data,
    NewData = {data, Tweets, sets:add_element(UserIdToSubscribeTo, Subscriptions), Followed_By},
    ManagerActor = whereis(manager_actor),
    ManagerActor ! {self(), get_data_actor, UserIdToSubscribeTo},
    io:format("Manager_actor_get 2 ~n"),
    receive
        {data_actor, DataActor} ->
            io:format("sub_rec_data_actor ~n"),
            DataActor ! {self(), follow_user, UserIdToSubscribeTo, UserId},
            receive
                {DataActor, followed_by_user, UserIdToSubscribeTo, UserId} -> NewData
            end
    end.

follow_user(Data, UserIdFollowing) ->
    io:format("followed ~n"),
    {data, Tweets, Subscriptions, Followed_By} = Data,
    NewData = {data, Tweets, Subscriptions, sets:add_element(UserIdFollowing, Followed_By)},
    NewData.

initialization_test() ->
    catch unregister(manager_actor),
    ?assertMatch(ok, initialize()).

register_user_test() ->
    initialization_test(),

    % We assume here that everything is sequential, and we have simple
    % incremental ids
    ?assertMatch({0, _Pid1}, register_user()),
    ?assertMatch({1, _Pid2}, register_user()),
    ?assertMatch({2, _Pid3}, register_user()),
    ?assertMatch({3, _Pid4}, register_user()).

init_for_test() ->
    catch unregister(manager_actor),
    initialize(),
    {0, Pid1} = register_user(),
    {1, Pid2} = register_user(),
    {2, Pid3} = register_user(),
    {3, Pid4} = register_user(),
    [Pid1, Pid2, Pid3, Pid4].

subscribe_to_test() ->
    io:format("Test"),
    [Pid1, Pid2, _, _] = init_for_test(),
    ?assertMatch(ok, subscribe(Pid1, 2, 1)).

