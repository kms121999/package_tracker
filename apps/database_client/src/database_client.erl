%% riak_client.erl
-module(database_client).
-export([connect/0, put/4, get/3, delete/3, disconnect/1]).

%% Simulate connecting to Riak
connect() ->
    %% Return a mock connection object (for example, a process ID or reference)
    io:format("Connecting to Riak database...~n"),
    Connection = riakc_pb_socket:start_link("127.0.0.1", 8087),
    io:format("Connection: ~p~n", [Connection]),
    % case riakc_pb_socket:ping(Pid) of
    % pong ->
    %     io:format("Connected to Riak server successfully~n");
    % _ ->
    %     io:format("Failed to connect to Riak server~n")
    % end,
    {ok,  make_ref()}.

%% Simulate putting data into Riak
put(Connection, Bucket, Key, Data) ->
    io:format("Saving data to Riak: Connection=~p, Bucket=~p, Key=~p, Data=~p~n", [Connection, Bucket, Key, Data]),
    ok.

%% Simulate getting data from Riak
get(Connection, Bucket, Key) ->
    %% Example of getting data (mocked for now)
    io:format("Getting data from Riak: Connection=~p, Bucket=~p, Key=~p~n", [Connection, Bucket, Key]),

    PackageData = #{
        <<"sender">> => <<"Alice">>, 
        <<"receiver">> => <<"Bob">>, 
        <<"destination">> => 
            #{ <<"street">> => <<"123 Cat Lane">>, 
                <<"city">> => <<"Wonderland">>, 
                <<"state">> => <<"NY">>, 
                <<"zip">> => <<"12345">>, 
                <<"country">> => <<"USA">> }, 
        <<"returnAddress">> => 
            #{ <<"street">> => <<"456 Yellow Brick Rd">>, 
                <<"city">> => <<"OZ">>, 
                <<"state">> => <<"KS">>, 
                <<"zip">> => <<"54321">>, 
                <<"country">> => <<"England">> }, 
        <<"status">> => <<"in transit">>, 
        <<"priority">> => <<"overnight">>, 
        <<"truckId">> => <<"truck123">>
    },

    PackageDataFakeTruck = maps:put(<<"truckId">>, <<"faketruck">>, PackageData),


    case {Bucket, Key} of
        {<<"trucks">>, <<"truck123">>} ->
            {ok, #{<<"long">> => -73.935242, <<"lat">> => 40.730610}};
        {<<"packages">>, <<"package123">>} ->
            {ok, PackageData};
        {<<"packages">>, <<"faketruck">>} ->
            {ok, PackageDataFakeTruck};
        _ ->
            {error, not_found}
    end.

%% Simulate deleting data from Riak
delete(Connection, Bucket, Key) ->
    io:format("Deleting data from Riak: Connection=~p, Bucket=~p, Key=~p~n", [Connection, Bucket, Key]),
    ok.

%% Simulate disconnecting from Riak
disconnect(Connection) ->
    io:format("Disconnecting from Riak with connection: ~p~n", [Connection]),
    ok.
