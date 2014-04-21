-module(server_single_actor).

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
    EntryPid ! {self(), subscribe, UserId, UserIdToSubscribeTo},
    receive
        {EntryPid, subscribed, UserId, UserIdToSubscribeTo} ->
            EntryPid ! {self(), follow_user, UserIdToSubscribeTo, UserId},
            receive
                {EntryPid, followed_by_user, UserIdToSubscribeTo, UserId} -> 
                    ok
            end
    end.

%%
%% Actors
%%

% 
manager_actor(Users) ->
    receive
        
        {Sender, register_user} ->
            {NewUsers, NewUserID, NewUserActor} = add_new_user(Users),
            Sender ! {registered_user, NewUserID, NewUserActor},
            manager_actor(NewUsers);
       
        {Sender, get_data_actor, UserId} ->
            Sender ! {data_actor, get_data_actor(Users, UserId)},
            manager_actor(Users)

        end.


data_actor(Data) ->
    receive


        {Sender, follow_user, UserId, UserIdFollowing} ->
            NewData = follow_user(Data, UserIdFollowing),
            Sender ! {self(), followed_by_user, UserId, UserIdFollowing},
            data_actor(NewData);
        
        {Sender, get_tweets, UserId, Page} ->
            Sender ! {self(), tweets, UserId, Page, tweets(Data, Page)},
            data_actor(Data);

        {Sender, tweet, UserId, Tweet} -> 
            {NewData, Timestamp} = tweet(Data, UserId, Tweet),
            Sender ! {self(), tweet_accepted, UserId, Timestamp},
            data_actor(NewData);

        {_Sender, tweet_to_follower, UserId, Timestamp, Tweet} ->
            NewData = subscribed_tweet(Data, UserId, Timestamp, Tweet),
            data_actor(NewData);
        
        {Sender, get_timeline, UserId, Page} ->
            Sender ! {self(), timeline, UserId, Page, timeline(Data, Page)},
            data_actor(Data);

        {Sender, subscribe, UserId, UserIdToSubscribeTo} ->
            NewData = subscribe_to_user(Data, UserIdToSubscribeTo),
            Sender ! {self(), subscribed, UserId, UserIdToSubscribeTo},
            data_actor(NewData)

            
    end.


entry_actor() ->
    ManagerActor = whereis(manager_actor),
    receive
        % RequestType ::= tweets | timeline
        {Sender, RequestType, UserId, PageOrTweetOrUserId} ->
            ManagerActor ! {self(), get_data_actor, UserId},
            receive
                {data_actor, DataActor} ->
                    DataActor ! {self(), RequestType, UserId, PageOrTweetOrUserId},

                    receive
                        {DataActor, ResponseType, UserId, Page, Result} ->
                            Sender ! {self(), ResponseType, UserId, Page, Result};
                        {DataActor, tweet_accepted, UserId, Timestamp} ->
                            Sender ! {self(), tweet_accepted, UserId, Timestamp};
                        {DataActor, subscribed, UserId, PageOrTweetOrUserId} ->
                            Sender ! {self(), subscribed, UserId, PageOrTweetOrUserId};
                        {DataActor, followed_by_user, UserId, PageOrTweetOrUserId} ->
                            Sender ! {self(), followed_by_user, UserId, PageOrTweetOrUserId}

                    end
            end

    end,
    entry_actor().

%%
%% Internal Functions
%%

get_data_actor(Users, UserId) ->
    {user, UserId, DataActor} = lists:nth(UserId + 1, Users),
    DataActor.

add_new_user(Users) ->
    NewUserId = length(Users),
    NewDataActor = spawn_link(?MODULE, data_actor, [{data, [], [], sets:new(), sets:new()}]), %% data, tweets, subscribedTweets, subscriptions, followed by
    NewUsers = Users ++ [{user, NewUserId, NewDataActor}],
    NewUserActor = spawn_link(?MODULE, entry_actor, []),
    {NewUsers, NewUserId, NewUserActor}.

subscribe_to_user(Data, UserIdToSubscribeTo) ->
    {data, Tweets, SubscribedTweets, Subscriptions, Followed_By} = Data,
    NewData = {data, Tweets, SubscribedTweets, sets:add_element(UserIdToSubscribeTo, Subscriptions), Followed_By},
    NewData.

follow_user(Data, UserIdFollowing) ->
    {data, Tweets, SubscribedTweets, Subscriptions, Followed_By} = Data,
    NewData = {data, Tweets, SubscribedTweets, Subscriptions, sets:add_element(UserIdFollowing, Followed_By)},
    NewData.

tweets(Data, _Page) ->
    {data, Tweets, _SubscribedTweets, _Subscriptions, _Followed_By} = Data,
    Tweets.

timeline(Data, _Page) ->
    {data, Tweets, SubscribedTweets, _Subscriptions, _Followed_By} = Data,
    SortedTweets = lists:reverse(lists:keysort(3, Tweets ++ SubscribedTweets)),
    SortedTweets.

tweet(Data, UserId, Tweet) ->
    {data, Tweets, SubscribedTweets, Subscriptions, Followed_By} = Data,
    Timestamp = erlang:now(),
    NewData = {data, Tweets ++ [{tweet, UserId, Timestamp, Tweet}], SubscribedTweets, Subscriptions, Followed_By},
    tweet_to_followers(sets:to_list(Followed_By), Tweet, Timestamp, UserId),
    {NewData, Timestamp}.

tweet_to_followers([], _, _, _) -> ok;
tweet_to_followers(Followers, Tweet, Timestamp, UserId) ->
    Follower = lists:nth(1, Followers),
    ManagerActor = whereis(manager_actor),
    ManagerActor ! {self(), get_data_actor, Follower},
    receive
        {data_actor, DataActor} ->
            DataActor ! {self(), tweet_to_follower, UserId, Timestamp, Tweet},
            tweet_to_followers(lists:nthtail(1, Followers), Tweet, Timestamp, UserId)
    end.

subscribed_tweet(Data, UserId, Timestamp, Tweet) ->
    {data, Tweets, SubscribedTweets, Subscriptions, Followed_By} = Data,
    NewData = {data, Tweets, SubscribedTweets ++ [{tweet, UserId, Timestamp, Tweet}], Subscriptions, Followed_By},
    NewData.
