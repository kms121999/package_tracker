%%%-------------------------------------------------------------------
%% @doc package_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(truck_update_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    truck_update_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
