
%% truck_data_retriever.erl
-module(package_get_server).
-behavior(gen_server).

%% API
-export([start_link/0, get_package_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%% Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initialize the state, and connect to database database
init([]) ->
    %% Create the connection to the database database (assuming database_client:connect/0 exists)
    case database_client:connect() of
        {ok, Connection} ->
            io:format("Connected to database database for retrieval with connection: ~p~n", [Connection]),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            io:format("Failed to connect to database database. Reason: ~p~n", [Reason]),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.


get_package_data(PackageId) ->
		%% Synchronous call to the gen_server to fetch package data
		gen_server:call(?MODULE, {get_package_data, PackageId}).

%% Handle synchronous calls
handle_call({get_package_data, PackageId}, _From, Connection) ->
    %% Query database for the truck location
    case database_client:get(Connection, <<"truck_locations">>, PackageId) of
        {ok, #{<<"longitude">> := Long, <<"latitude">> := Lat}} ->
            %% Found the truck location, return it
            {reply, {ok, Long, Lat}, Connection};
        {error, not_found} ->
            %% Handle the case where the truck location is not found
            io:format("Truck ~p not found in database~n", [PackageId]),
            {reply, {error, not_found}, Connection};
        {error, Reason} ->
            %% General error handling
            io:format("Error retrieving truck ~p from database. Reason: ~p~n", [PackageId, Reason]),
            {reply, {error, Reason}, Connection}
    end.

%% Handle the server termination (clean up)
terminate(_Reason, Connection) ->
    %% Close the database connection
    database_client:disconnect(Connection),
    ok.

%% Handle code upgrades (if needed)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.







%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% -include_lib("meck/include/meck.hrl").
 




%% Test for package retrieval success and failure using mocked database_client
package_retrieval_test_() ->
    %% Setup test context
    {setup,
        fun setup/0, 
        fun cleanup/1, 
        [
            fun test_package_found/0
            % fun test_package_not_found/0,
            % fun test_database_error/0
        ]
    }.

test_test_() ->
		?assertEqual(1, 1).

%% Setup function to mock database_client before each test
setup() ->
    %% Start mocking the database_client module
    meck:new(database_client),
    
    %% Mock the connect function to always succeed
    meck:expect(database_client, connect, 0, {ok, mock_connection}),
    
    %% Mock the disconnect function
    meck:expect(database_client, disconnect, 1, ok),
    
    %% Start the truck_data_retriever service
    {ok, Pid} = truck_data_retriever:start_link(),
    
    %% Return the Pid to use in cleanup
    Pid.

%% Cleanup function to unload the mocks
cleanup(Pid) ->
    %% Stop the truck_data_retriever process
    gen_server:stop(Pid),
    
    %% Unload the meck mock for database_client
    meck:unload(database_client).

test_package_found()->
    PackageData = #{
        <<"sender">> => <<"Alice">>,
        <<"receiver">> => <<"Bob">>,
        <<"destination">>=> #{
            <<"street">>=> <<"123 Cat Lane">>,
            <<"city">>=> <<"Wonderland">>,
          <<"state">>=> <<"NY">>,
            <<"zip">>=> <<"12345">>,
          <<"country">>=> <<"USA">>
        },
        <<"returnAddress">>=> #{
            <<"street">>=> <<"456 Yellow Brick Rd">>,
            <<"city">>=> <<"OZ">>,
          <<"state">>=> <<"KS">>,
            <<"zip">>=> <<"54321">>,
          <<"country">>=> <<"England">>
        },
        <<"status">>=> <<"in transit">>,
        <<"priority">>=> <<"overnight">>,
        <<"truckId">>=> <<"truck123">>,
        <<"longitude">>=> <<-72.532>>,
        <<"latitude">>=> <<42.532>>
    },
    DatabaseError = {error, "Database down"},
	 %% Mock the get function to return package data when requested
    meck:expect(database_client, get, 3, 
        fun (_Connection, <<"packages">>, <<"PKG123456">>) ->
            {ok, PackageData}
        end
	),
	[% happy thoughts
   ?_assertEqual(PackageData, get_package_data(<<"package123">>)),
	 % nasty thoughts start here
	 ?_assertEqual({error, not_found}, get_package_data(<<"fakepackage">>)),
	 ?_assertEqual(DatabaseError, get_package_data(<<"databasedown">>))
	].



-endif.