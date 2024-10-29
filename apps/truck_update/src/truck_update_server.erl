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