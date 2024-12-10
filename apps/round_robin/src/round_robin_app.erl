%%%-------------------------------------------------------------------
%% @doc database client public API
%% @end
%%%-------------------------------------------------------------------

-module(round_robin_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    round_robin_sup:start_link().

stop(_State) ->
    ok.

%% internal functions