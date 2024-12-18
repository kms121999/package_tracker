
%% package_get_server.erl
-module(package_get_server).
-behavior(gen_server).

%% API
-export([start_link/0, get_package_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Start the server
start_link() ->
    lumberjack_server:info("Starting gen_server", #{module => ?MODULE}),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initialize the state, and connect to database database
init([]) ->
    lumberjack_server:info("Initializing gen_server", #{module => ?MODULE}),
    %% Create the connection to the database database (assuming database_client:connect/0 exists)
    case database_client:connect() of
        {ok, Connection} ->
            lumberjack_server:info("Connected to database", #{module => ?MODULE, connection => Connection}),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            lumberjack_server:error("Failed to connect to database", #{module => ?MODULE, reason => Reason}),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.


get_package_data(PackageId, ReqId) ->
        lumberjack_server:info("Calling for package data", #{module => ?MODULE, package_id => PackageId, req_id => ReqId}),
		%% Synchronous call to the gen_server to fetch package data
		gen_server:call({?MODULE, 'backend@backend.keatonsmith.com'}, {get_package_data, PackageId, ReqId}).

%% Handle synchronous calls
handle_call({get_package_data, PackageId, ReqId}, _From, Connection) ->
    lumberjack_server:info("Retrieving package data", #{module => ?MODULE, package_id => PackageId, req_id => ReqId}),
    %% Query database for the package
    case database_client:get(Connection, <<"packages">>, PackageId) of
        {ok, Data} ->
            %% Found the package data, return it
            TruckId = maps:get(<<"truckId">>, Data),
            lumberjack_server:info("Retrieving truck data", #{module => ?MODULE, truck_id => TruckId, req_id => ReqId}),
            case database_client:get(Connection, <<"trucks">>, TruckId) of
                {ok, TruckData} ->
                    lumberjack_server:info("Truck data retrieved", #{module => ?MODULE, req_id => ReqId}),
                    {reply, {ok, maps:put(<<"location">>, TruckData, Data)}, Connection};
                {error, notfound} ->
                    lumberjack_server:warning("Truck data not found", #{module => ?MODULE, truck_id => TruckId, req_id => ReqId}),
                    {reply, {ok, maps:put(<<"location">>, null, Data)}, Connection};
                {error, Reason} ->
                    lumberjack_server:error("Error retrieving truck data", #{module => ?MODULE, truck_id => TruckId, reason => Reason, req_id => ReqId}),
                    {reply, {error, Reason}, Connection}
            end;
        {error, notfound} ->
            %% Handle the case where the package is not found
            lumberjack_server:warning("Package not found", #{module => ?MODULE, package_id => PackageId, req_id => ReqId}),
            {reply, {error, notfound}, Connection};
        {error, Reason} ->
            %% General error handling
            lumberjack_server:error("Error retrieving package", #{module => ?MODULE, package_id => PackageId, reason => Reason, req_id => ReqId}),
            {reply, {error, Reason}, Connection}
    end.

handle_cast(Msg, Connection) ->
    lumberjack_server:warning("Unimplemented method called: handle_cast", #{module => ?MODULE, message => Msg}),
    {noreply, Connection}.

%% Handle the server termination (clean up)
terminate(_Reason, Connection) ->
    %% Close the database connection
    lumberjack_server:info("Terminating gen_server", #{module => ?MODULE}),
    lumberjack_server:info("Disconnecting from database", #{module => ?MODULE}),
    database_client:disconnect(Connection),
    ok.




%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test for package retrieval success and failure using mocked database_client
package_retrieval_test_() ->
    %% Setup test context
    {setup,
        fun setup/0, 
        fun cleanup/1, 
        [
            fun test_package_found/0
        ]
    }.

%% Setup function to mock database_client before each test
setup() ->
    %% Start mocking the database_client module
    meck:new(database_client),
    
    %% Mock the connect function to always succeed
    meck:expect(database_client, connect, 0, {ok, mock_connection}),
    
    %% Mock the disconnect function
    meck:expect(database_client, disconnect, 1, ok),

    meck:new(lumberjack_server),
    meck:expect(lumberjack_server, info, 2, ok),
    meck:expect(lumberjack_server, warning, 2, ok),
    meck:expect(lumberjack_server, error, 2, ok),
    
    %% Start the package_get_server service
    {ok, Pid} = package_get_server:start_link(),
    
    %% Return the Pid to use in cleanup
    Pid.

%% Cleanup function to unload the mocks
cleanup(Pid) ->
    %% Stop the package_get_server process
    gen_server:stop(Pid),
    
    %% Unload the meck mock for database_client
    meck:unload(database_client),
    meck:unload(lumberjack_server).

test_package_found()->
    StoredPackageData = #{
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

    FinalPackageData = maps:put(<<"location">>, #{
        <<"long">> => -72.532, 
        <<"lat">> => 42.532
    }, StoredPackageData),

    StoredPackageDataBadTruck = maps:put(<<"truckId">>, <<"bad_truck">>, StoredPackageData),
    FinalPackageDataBadTruck = maps:put(<<"location">>, null, StoredPackageDataBadTruck),

    StoredPackageDataTruckDatabaseDown = maps:put(<<"truckId">>, <<"databasedown">>, StoredPackageData),
    
	 %% Mock the get function to return package data when requested
    meck:expect(database_client, get, 3, 
        fun (_Connection, <<"packages">>, <<"package123">>) ->
                {ok, StoredPackageData};
            (_Connection, <<"packages">>, <<"package_with_bad_truck">>) ->
                {ok, StoredPackageDataBadTruck};
            (_Connection, <<"packages">>, <<"package_with_failed_truck_get">>) ->
                {ok, StoredPackageDataTruckDatabaseDown};
            (_Connection, <<"packages">>, <<"bad_package">>) ->
                {error, notfound};
            (_Connection, <<"packages">>, <<"databasedown">>) ->
                {error, "Database down"};

            (_Connection, <<"trucks">>, <<"truck123">>) ->
                {ok, maps:get(<<"location">>, FinalPackageData)};
            (_Connection, <<"trucks">>, <<"bad_truck">>) ->
                {error, notfound};
            (_Connection, <<"trucks">>, <<"databasedown">>) ->
                {error, "Database down"}
        end
	),

	% happy thoughts
    ?assertEqual({reply, {ok, FinalPackageData}, mock_connection}, handle_call({get_package_data, <<"package123">>, "req123"}, any, mock_connection)),
    ?assertEqual({reply, {ok, FinalPackageDataBadTruck}, mock_connection}, handle_call({get_package_data, <<"package_with_bad_truck">>, "req123"}, any, mock_connection)),
    % nasty thoughts start here
    ?assertEqual({reply, {error, notfound}, mock_connection}, handle_call({get_package_data, <<"bad_package">>, "req123"}, any, mock_connection)),
    ?assertEqual({reply, {error, "Database down"}, mock_connection}, handle_call({get_package_data, <<"package_with_failed_truck_get">>, "req123"}, any, mock_connection)),
    ?assertEqual({reply, {error, "Database down"}, mock_connection}, handle_call({get_package_data, <<"databasedown">>, "req123"}, any, mock_connection)).

-endif.