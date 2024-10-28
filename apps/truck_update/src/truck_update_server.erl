%% truck_update.erl
-module(truck_update_server).
-behaviour(gen_server).

-export([start_link/0, update_location/3]).
-export([init/1, handle_call/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_location(TruckID, Lat, Long) ->
    gen_server:call(?MODULE, {update, TruckID, Lat, Long}).

init([]) ->
    {ok, #{}}.

handle_call({update, TruckID, Lat, Long}, _From, State) ->
    %% Simulate interaction with db_client here
    case db_client:find_truck(TruckID) of
        {ok, Truck} ->
            UpdatedTruck = Truck#{latitude => Lat, longitude => Long},
            ok = db_client:update_truck(UpdatedTruck),
            {reply, {ok, updated}, State};
        error ->
            NewTruck = #{id => TruckID, latitude => Lat, longitude => Long},
            ok = db_client:insert_truck(NewTruck),
            {reply, {ok, inserted}, State}
    end.