%%%-------------------------------------------------------------------
%% @doc lumberjack public API
%% @end
%%%-------------------------------------------------------------------

-module(lumberjack_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lumberjack_sup:start_link().

stop(_State) ->
    ok.

%% internal functions