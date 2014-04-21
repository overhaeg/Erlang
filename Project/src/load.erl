%%%-------------------------------------------------------------------
%%% @author Christophe De Troyer
%%% @copyright (C) 2014
%%% @doc
%%% Module that will run a few load tests on the Erlang project
%%% @end
%%% Created : 11. Mar 2014 5:07 PM
%%%-------------------------------------------------------------------

% TODO LIST
% TODO Proper print server load params
-module(load).
-author("Christophe De Troyer").

% Compile flags
-compile([debug_info]).

%**************************************************************************************************
% Run from terminal:
% erl -noshell -eval 'load:start(50,10,5,1,1, 5)' -eval 'init:stop()'

%**************************************************************************************************
% Loop the server
-export([% Pubilc
  start/6,
  % Private
  get_timeline_for_all_users/2, get_timeline_for_user/2, register_users/2,
  subscribe_to_users/3, get_tweets_for_all_users/2, get_tweets_for_user/2,
  spam_tweets/2, user_tweet_loop/2, wait_for_tweets_finish/1, begin_load/1]).

%**************************************************************************************************
% This is the function that will start the load

start(NumberOfUsers, NumberOfSubscriptions, TweetsPerUser, TimelineReqsPerUser, AllTweetsReqPerUser, MaximumUsersPerDataActor) ->
  % Create dictionary with the parameters
  Params = [{number_of_users, NumberOfUsers}, {number_of_subscriptions, NumberOfSubscriptions},
            {tweets_per_user, TweetsPerUser}, {tl_req_per_user, TimelineReqsPerUser},
            {alltweets_req_per_user, AllTweetsReqPerUser}, {users_per_dataactor, MaximumUsersPerDataActor}],
  ParamDict = dict:from_list(Params),
  begin_load(ParamDict).


begin_load(ParamDict) ->
    register(shell, self()),
    List = setup_server(ParamDict),
    benchmark_server(List, ParamDict),
    ok.

total_tweets_stored() ->
    data_actor_manager ! {self(), count_total_tweets},
    receive
      {total_tweets, Total} -> Total
    end,
    ok.

%**************************************************************************************************
% Initiate Server

% This code is specific to the implementation of the backend. So it is probably best changed by 
% the user of this code.
setup_server(ParamDict) ->
    printLoadTestParameters(dict:fetch(number_of_users, ParamDict), dict:fetch(number_of_subscriptions, ParamDict), dict:fetch(tweets_per_user, ParamDict)),
    % Setup the sever back end
    server_single_actor:initialize(),
    % Register NUMBER_OF_USERS with the data_actor_manager
    {Time, UserEntryPids} = timer:tc(?MODULE, register_users, [dict:fetch(number_of_users, ParamDict), []]),
    io:fwrite("~p * Users registered in ~p seconds~n", [self(), Time/ 1000000]),
    
    % Subscribe to random users
    subscribe_to_random_users(UserEntryPids, dict:fetch(number_of_users, ParamDict), dict:fetch(number_of_subscriptions, ParamDict)),
    io:fwrite("~p * All subscriptions made!~n", [self()]),
    UserEntryPids.
 
%**************************************************************************************************
% Server load
benchmark_server(UserList, ParamDict) ->
    {Time, _} = timer:tc(?MODULE, spam_tweets, [UserList, ParamDict]),
    io:fwrite("~p * Sent  tweets in ~p seconds~n", [self(), Time/ 1000000]),

    {Time2, _} = timer:tc(?MODULE, get_tweets_for_all_users, [UserList, ParamDict]),
    io:fwrite("~p * Got all tweets for all users in ~p seconds~n", [self(), Time2/ 1000000]),

    {Time3, _} = timer:tc(?MODULE, get_timeline_for_all_users, [UserList, ParamDict]),
    io:fwrite("~p * Got timeline for all users in ~p seconds~n", [self(), Time3/ 1000000]),

    %("Writing stats to file..~n"),
    % Server parameters and statistics
    % TotalTweetsOnServer          = total_tweets_stored(),
    NumberOfSchedulers           = erlang:system_info(schedulers_online),
    NumberOfTweetsPerUser        = dict:fetch(tweets_per_user, ParamDict),
    NumberOfUsers                = dict:fetch(number_of_users, ParamDict),
    SubscriptionsPerUser         = dict:fetch(number_of_subscriptions, ParamDict),
    TotalTimelineRequestsPerUser = dict:fetch(tl_req_per_user, ParamDict),
    TotalTweetsRequetsPerUser    = dict:fetch(alltweets_req_per_user, ParamDict),
    UserPerDataActor             = dict:fetch(users_per_dataactor, ParamDict),
    % Write statistics to output file
    % writeStatsToFile(NumberOfCores, NumberOfTweets, NumberOfUsers, Miliseconds) 
    write_to_file:writeTweetPerformance(NumberOfSchedulers,
                                        NumberOfTweetsPerUser,
                                        NumberOfUsers,
                                        UserPerDataActor,
                                        Time / 1000),
    write_to_file:writeFetchTweetsReqPerformance(
                                        NumberOfSchedulers,
                                        NumberOfTweetsPerUser,
                                        NumberOfUsers,
                                        TotalTweetsRequetsPerUser,
                                        UserPerDataActor,
                                        Time2 / 1000),
    write_to_file:writeFetchTimelineReqPerformance(
                                        NumberOfSchedulers,
                                        NumberOfTweetsPerUser,
                                        NumberOfUsers,
                                        TotalTimelineRequestsPerUser,
                                        SubscriptionsPerUser,
                                        UserPerDataActor,
                                        Time3 / 1000),   

    io:fwrite("~p * Benchmarking done! ~n", [self()]),
    ok.

%**************************************************************************************************
% Register Users

%Function that loops until the number of registrations is met
register_users(0, UserEntryPids) -> UserEntryPids;
register_users(N, UserEntryPids) ->
  NewUser = server_single_actor:register_user(),
  register_users(N - 1, [NewUser | UserEntryPids]).

%**************************************************************************************************
% Subscribe to users

%Subscribe to random users using a random generated list
subscribe_to_random_users([], NumberOfUsers, _) ->
  waitForSubscriptionsToFinish(NumberOfUsers),
  ok;
subscribe_to_random_users([User | Rest], NumberOfUsers, NumberOfSubscriptions) ->
  UsersToSubscribeTo = helpers:createRandomList(0, NumberOfUsers - 1, NumberOfSubscriptions),
  spawn(?MODULE, subscribe_to_users, [UsersToSubscribeTo, User, self()]),
  subscribe_to_random_users(Rest, NumberOfUsers, NumberOfSubscriptions).

waitForSubscriptionsToFinish(0) -> ok;
waitForSubscriptionsToFinish(N) ->
  receive
    {done_subscribing} -> waitForSubscriptionsToFinish(N -1)
  end.

subscribe_to_users([],{_UserId, _EntryPid}, CommanderPid) ->
  CommanderPid ! {done_subscribing},
  ok;
subscribe_to_users([UserIdToSubscribeTo | SubscribeTos], {UserId, EntryPid}, CommanderPid) ->
  server_single_actor:subscribe(EntryPid, UserId, UserIdToSubscribeTo),
  subscribe_to_users(SubscribeTos, {UserId, EntryPid}, CommanderPid).



%**************************************************************************************************
% Request the timeline for each user

get_timeline_for_all_users(UserList, ParamDict) ->
    lists:map(fun(User) ->
            spawn(?MODULE, get_timeline_for_user, [User, dict:fetch(tl_req_per_user, ParamDict)])
          end,
          UserList),
    wait_for_finish_get_timeline(length(UserList)),
    ok.

get_timeline_for_user(_, 0) ->
    shell ! {got_timeline},
    ok;
get_timeline_for_user({UserId, EntryPid}, N) -> 
    Tweets = server:get_timeline(EntryPid, UserId, 1),
    io:fwrite("Timeline: ~p~n", [length(Tweets)]),
    get_timeline_for_user({UserId, EntryPid}, N -1).


wait_for_finish_get_timeline(0) -> ok;
wait_for_finish_get_timeline(N) ->
    receive
        {got_timeline} -> wait_for_finish_get_timeline(N -1)
    end. 

%**************************************************************************************************
% Get All tweets by a certain user

get_tweets_for_all_users(UserList, ParamDict) ->
    lists:map(fun(User) ->
            spawn(?MODULE, get_tweets_for_user, [User, dict:fetch(alltweets_req_per_user, ParamDict)])
          end,
          UserList),
    wait_for_finish_get_tweets(dict:fetch(number_of_users, ParamDict)),
    ok.

get_tweets_for_user(_, 0) -> 
    shell ! {got_tweets},
    ok;
get_tweets_for_user({UserId, EntryPid}, N) -> 
    Tweets = server:get_tweets(EntryPid, UserId, 1),
    io:fwrite("Tweets: ~p~n", [length(Tweets)]),
    get_tweets_for_user({UserId, EntryPid}, N -1).
    

wait_for_finish_get_tweets(0) -> ok;
wait_for_finish_get_tweets(N) ->
    receive
        {got_tweets} -> wait_for_finish_get_tweets(N -1)
    end. 


%***************************************** Tweet Spamming   ***************************************
% Spam the server with a lot of tweets
spam_tweets(UserList, ParamDict) ->
    lists:map(fun(User) ->
                spawn(?MODULE, user_tweet_loop, [User,dict:fetch(tweets_per_user, ParamDict)])
              end,
              UserList),

    wait_for_tweets_finish(dict:fetch(number_of_users, ParamDict)),
    ok.

wait_for_tweets_finish(0) -> ok;
wait_for_tweets_finish(N) ->
    receive
        {done_spawning_tweeters} -> wait_for_tweets_finish(N -1)
    end.    

user_tweet_loop(_, 0) ->
    shell ! {done_spawning_tweeters};

user_tweet_loop({UserId, EntryPid}, Counter) ->
    _Timestamp = server:tweet(EntryPid, UserId, io_lib:fwrite("Tweet from ~p. (~p)", [UserId, Counter])),
    user_tweet_loop({UserId, EntryPid}, Counter - 1).


%**************************************************************************************************
% Print parameters

printLoadTestParameters(NumberOfUsers, NumberToSubscribeTo, TweetsPerUsr) ->
  io:fwrite("~p * Initializing server:
             -> Number of schedulers: ~p
             -> Number of users: ~p
             -> Each user will subscribe to ~p users
             -> Each user will tweet ~p tweets~n",
    [self(),erlang:system_info(schedulers_online), NumberOfUsers, NumberToSubscribeTo, TweetsPerUsr]).    
