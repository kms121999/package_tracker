%%%-------------------------------------------------------------------
%% @doc package_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(package_get_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    package_get_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
