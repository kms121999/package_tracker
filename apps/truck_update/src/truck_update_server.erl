%% truck_update.erl
-module(truck_update_server).
-behavior(gen_server).

-export([start_link/0, update_location/3]).
-export([init/1, handle_cast/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_location(TruckID, Lat, Long) ->
    gen_server:cast(?MODULE, {update, TruckID, Lat, Long}).

init([]) ->
    case database_client:connect() of
        {ok, Connection} ->
            io:format("Connected to database database for retrieval with connection: ~p~n", [Connection]),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            io:format("Failed to connect to database database. Reason: ~p~n", [Reason]),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.

handle_cast({update, TruckID, Lat, Long}, _From, Connection) ->
    %% Simulate interaction with db_client here
    case database_client:get(Connection, <<"trucks">>, TruckID) of
        {ok, Truck} ->
            UpdatedTruck = Truck#{latitude => Lat, longitude => Long},
            case database_client:put(Connection, <<"trucks">>, TruckID, UpdatedTruck) of
                {ok, _Updated} ->
                    lumberjack_server:info("Truck data updated", #{module => ?MODULE});
                {error, Reason} ->
                    lumberjack_server:error("Error updating truck data", #{module => ?MODULE, truckId => TruckID, reason => Reason})
                end;
        {error, not_found} ->
            NewTruck = #{<<"long">> => Long, <<"lat">> => Lat},
            case database_client:put(Connection, <<"trucks">>, TruckID, NewTruck) of
                {ok, _Inserted} ->
                    lumberjack_server:info("Truck inserted", #{module => ?MODULE});
                {error, _Reason} ->
                    lumberjack_server:error("Error updating truck data", #{module => ?MODULE, truckId => TruckId, reason => Reason})
                end
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
 




%% Test for truck update success and failure using mocked database_client
truck_update_test_() ->
    %% Setup test context
    {setup,
        fun setup/0, 
        fun cleanup/1, 
        [
            fun test_truck_update/0
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
    
    %% Start the truck_update_server service
    {ok, Pid} = truck_update_server:start_link(),
    
    %% Return the Pid to use in cleanup
    Pid.

%% Cleanup function to unload the mocks
cleanup(Pid) ->
    %% Stop the truck_update_server process
    gen_server:stop(Pid),
    
    %% Unload the meck mock for database_client
    meck:unload(database_client).

test_truck_update()->
    
	 %% Mock the get function to return truck data when requested
    meck:expect(database_client, put, 4, 
        fun (_Connection, <<"trucks">>, <<"truck123">>, _Data) ->
                {ok, updated};
            (_Connection, <<"trucks">>, <<"truck321">>, _Data) ->
                {ok, inserted};
            (_Connection, <<"trucks">>, <<"databasedown">>, _Data) ->
                {error, "Database down"}
        end
	),

	% happy thoughts
    ?assertEqual({ok, updated}, update_location(<<"truck123">>, -72.532, 42.532)),
	?assertEqual({ok, inserted}, update_location(<<"truck321">>, -72.532, 42.532)),
    % nasty thoughts start here
	?assertEqual({error, "Database down"}, update_location(<<"databasedown">>, -72.532, 42.532)).


-endif.