
%% truck_data_retriever.erl
-module(truck_data_retriever).
-behaviour(gen_server).

%% API
-export([start_link/0, get_truck_location/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%% Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initialize the state, and connect to Riak database
init([]) ->
    %% Create the connection to the Riak database (assuming riak_client:connect/0 exists)
    case riak_client:connect() of
        {ok, Connection} ->
            io:format("Connected to Riak database for retrieval with connection: ~p~n", [Connection]),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            io:format("Failed to connect to Riak database. Reason: ~p~n", [Reason]),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.


get_package_data(PackageId) ->
		%% Synchronous call to the gen_server to fetch package data
		gen_server:call(?MODULE, {get_package_data, PackageId}).

%% Handle synchronous calls
handle_call({get_package_data, PackageId}, _From, Connection) ->
    %% Query Riak for the truck location
    case database_client:get(Connection, <<"truck_locations">>, PackageId) of
        {ok, #{<<"longitude">> := Long, <<"latitude">> := Lat}} ->
            %% Found the truck location, return it
            {reply, {ok, Long, Lat}, Connection};
        {error, not_found} ->
            %% Handle the case where the truck location is not found
            io:format("Truck ~p not found in Riak~n", [TruckId]),
            {reply, {error, not_found}, Connection};
        {error, Reason} ->
            %% General error handling
            io:format("Error retrieving truck ~p from Riak. Reason: ~p~n", [TruckId, Reason]),
            {reply, {error, Reason}, Connection}
    end.

%% Handle the server termination (clean up)
terminate(_Reason, Connection) ->
    %% Close the Riak connection
    riak_client:disconnect(Connection),
    ok.

%% Handle code upgrades (if needed)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.







%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
	PackageData = #{
			<<"sender">>: <<"Alice">>,
			<<"receiver">>: <<"Bob">>,
			<<"destination">>: #{
				<<"street">>: <<"123 Cat Lane">>,
				<<"city">>: <<"Wonderland">>
			  <<"state">>: <<"NY">>,
				<<"zip">>: <<"12345">>
			  <<"country">>: <<"USA">>
			},
			<<"returnAddress">>: #{
				<<"street">>: <<"456 Yellow Brick Rd">>,
				<<"city">>: <<"OZ">>
			  <<"state">>: <<"KS">>,
				<<"zip">>: <<"54321">>
			  <<"country">>: <<"England">>
			},
			<<"status">>: <<"in transit">>,
			<<"priority">>: <<"overnight">>
			<<"truckId">>: <<"truck123">>,
			<<"longitude">>: <<-72.532>>,
			<<"latitude">>: <<42.532>>
	}

DatabaseError = {error, "Database down"}

_test_()->
	[% happy thoughts
   ?_assertEqual(PackageData, get_package_data(<<"package123">>)),
	 % nasty thoughts start here
	 ?_assertEqual({error, not_found}, get_package_data(<<"fakepackage">>)),
	 ?_assertEqual(DatabaseError, get_package_data(<<"databasedown">>))
	].

-endif.