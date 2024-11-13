%%%-------------------------------------------------------------------
%% @doc database client public API
%% @end
%%%-------------------------------------------------------------------

-module(database_client_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    database_client_sup:start_link().

stop(_State) ->
    ok.

%% internal functions