%%%-------------------------------------------------------------------
%% @doc package_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(package_tracker_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Middlewares = [
        req_id_middleware,
        logger_middleware,
        cowboy_router,
        cowboy_handler
    ],

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/package/get", package_get_handler, []},
            {"/truck/update", truck_update_handler, []},
            {"/package/update", package_update_handler, []}
        ]}
        % {"packages.localhost", [{"/:package_id", package_get_handler, []}]}
    ]),

    {ok, _} = cowboy:start_clear(http_listener, [{port, 80}], #{
        env => #{dispatch => Dispatch},
        middlewares => Middlewares
    }),

    % TODO, setup ssl
    % PrivDir = code:priv_dir(silly_server),
    % {ok, _} = cowboy:start_tls(https_listener, [
    %     {port, 443},
    %     {certfile, filename:join([PrivDir, "ssl", "server.crt"])},
    %     {keyfile, filename:join([PrivDir, "ssl", "server.key"])}
    % ]),

    package_tracker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
