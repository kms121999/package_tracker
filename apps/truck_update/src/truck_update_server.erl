%% truck_update.erl
-module(truck_update_server).
-behavior(gen_server).

-export([start_link/0, update_location/3]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_location(TruckId, Lat, Long) ->
    gen_server:cast({?MODULE, 'backend@backend.keatonsmith.com'}, {update, TruckId, Lat, Long}).

init([]) ->
    case database_client:connect() of
        {ok, Connection} ->
            io:format("Connected to database for retrieval with connection: ~p~n", [Connection]),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            io:format("Failed to connect to database database. Reason: ~p~n", [Reason]),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.

handle_cast({update, TruckId, Lat, Long}, Connection) ->
    %% Simulate interaction with db_client here
    UpdatedTruck = #{<<"long">> => Long, <<"lat">> => Lat},
    case database_client:put(Connection, <<"trucks">>, TruckId, UpdatedTruck) of
        ok ->
            lumberjack_server:info("Truck updated", #{module => ?MODULE}),
            {noreply, Connection};
        {error, Reason} ->
            lumberjack_server:error("Error updating truck data", #{module => ?MODULE, truckId => TruckId, reason => Reason}),
            {noreply, Connection}
        end.

handle_call(Msg, From, Connection) ->
    lumberjack_server:warning("Unimplemented method called: handle_call", #{module => ?MODULE, from => From, message => Msg}),
    {reply, {error, unimplemented}, Connection}.

terminate(_Reason, Connection) ->
    %% Close the database connection
    database_client:disconnect(Connection),
    ok.



%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 

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

setup() ->
    meck:new(lumberjack_server),
    meck:expect(lumberjack_server, info, 2, ok),
    meck:expect(lumberjack_server, warning, 2, ok),
    meck:expect(lumberjack_server, error, 2, ok),

    %% Start mocking the database_client module
    meck:new(database_client),
    
    %% Mock the connect function to always succeed
    meck:expect(database_client, connect, 0, {ok, mock_connection}),
    
    %% Mock the disconnect function
    meck:expect(database_client, disconnect, 1, ok),

    % %% Mock the put function to simulate database updates and inserts
    meck:expect(database_client, put, 4, 
        fun (_Connection, <<"trucks">>, <<"truck123">>, _Data) ->
                ok;
            (_Connection, <<"trucks">>, <<"newtruck">>, _Data) ->
                ok;
            (_Connection, <<"trucks">>, <<"databasedown">>, _Data) ->
                {error, "Database down"}
        end
	),

    %% Start the truck_update_server service
    {ok, Pid} = truck_update_server:start_link(),

    %% Return the Pid to use in cleanup
    Pid.


%% Cleanup function to unload the mocks
cleanup(Pid) ->
    %% Stop the truck_update_server process
    gen_server:stop(Pid),
    
    %% Unload the meck mock for database_client
    meck:unload(lumberjack_server),
    meck:unload(database_client).

test_truck_update()->
    
	% happy thoughts
    ?assertEqual({noreply, mock_connection}, handle_cast({update, <<"truck123">>, -72.532, 42.532}, mock_connection)),
	?assertEqual({noreply, mock_connection}, handle_cast({update, <<"newtruck">>, -72.532, 42.532}, mock_connection)),
    % nasty thoughts start here
	?assertEqual({noreply, mock_connection}, handle_cast({update, <<"databasedown">>, -72.532, 42.532}, mock_connection)).

-endif.