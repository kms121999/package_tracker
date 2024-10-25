-module(truck_update_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, []}.


handle_cast({update_truck, TruckId, NewData}, _From, State) ->

    case database_client:put(State, <<"trucks">>, TruckId, NewData) of
        {ok, _} ->
            {noreply, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.