%% truck_update.erl
-module(truck_update_server).
-behavior(gen_server).

-export([start_link/0, update_location/3]).
-export([init/1, handle_call/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_location(TruckID, Lat, Long) ->
    gen_server:call(?MODULE, {update, TruckID, Lat, Long}).

init([]) ->
    case database_client:connect() of
        {ok, Connection} ->
            io:format("Connected to database database for retrieval with connection: ~p~n", [Connection]),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            io:format("Failed to connect to database database. Reason: ~p~n", [Reason]),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.

handle_call({update, TruckID, Lat, Long}, _From, Connection) ->
    %% Simulate interaction with db_client here
    case database_client:get(Connection, <<"trucks">>, TruckID) of
        {ok, Truck} ->
            UpdatedTruck = Truck#{latitude => Lat, longitude => Long},
            ok = database_client:put(Connection, <<"trucks">>, TruckID, UpdatedTruck),
            {reply, {ok, updated}, Connection};
        {error, not_found} ->
            NewTruck = #{id => TruckID, latitude => Lat, longitude => Long},
            ok = database_client:put(Connection, <<"trucks">>, TruckID, NewTruck),
            {reply, {ok, inserted}, Connection}
    end.

terminate(_Reason, Connection) ->
    %% Close the database connection
    database_client:disconnect(Connection),
    ok.



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
    
    %% Start the package_get_server service
    {ok, Pid} = package_get_server:start_link(),
    
    %% Return the Pid to use in cleanup
    Pid.

%% Cleanup function to unload the mocks
cleanup(Pid) ->
    %% Stop the package_get_server process
    gen_server:stop(Pid),
    
    %% Unload the meck mock for database_client
    meck:unload(database_client).

test_package_found()->
    PackageData = #{
        sender => "Alice", 
        receiver => "Bob", 
        destination => 
            #{ street => "123 Cat Lane", 
                city => "Wonderland", 
                state => "NY", 
                zip => "12345", 
                country => "USA" }, 
        returnAddress => 
            #{ street => "456 Yellow Brick Rd", 
                city => "OZ", 
                state => "KS", 
                zip => "54321", 
                country => "England" }, 
        status => "in transit", 
        priority => "overnight", 
        truckId => "truck123", 
        longitude => "-72.532", 
        latitude => "42.532"
    },
    
	 %% Mock the get function to return package data when requested
    meck:expect(database_client, get, 3, 
        fun (_Connection, <<"packages">>, <<"package123">>) ->
                {ok, PackageData};
            (_Connection, <<"packages">>, <<"fakepackage">>) ->
                {error, not_found};
            (_Connection, <<"packages">>, <<"databasedown">>) ->
                {error, "Database down"}
        end
	),

	% happy thoughts
    ?assertEqual({ok, PackageData}, get_package_data(<<"package123">>)),
    % nasty thoughts start here
	?assertEqual({error, not_found}, get_package_data(<<"fakepackage">>)),
	?assertEqual({error, "Database down"}, get_package_data(<<"databasedown">>)).

-endif.