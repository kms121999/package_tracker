%%%-------------------------------------------------------------------
%% @doc logger public API
%% @end
%%%-------------------------------------------------------------------

-module(logger_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger_sup:start_link().

stop(_State) ->
    ok.

%% internal functions