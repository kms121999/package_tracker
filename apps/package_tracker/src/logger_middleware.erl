-module(logger_middleware).
-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    %% Get request id
    ReqId = maps:get(req_id, maps:get(package_tracker, Req)),

    % Log request
    lumberjack_server:info("Received request", #{
      module => ?MODULE,
      req_id => ReqId,
      peer_ip => cowboy_req:peer(Req),
      method => cowboy_req:method(Req),
      path => cowboy_req:path(Req)
    }),
    %% Pass the modified request to the next middleware or handler
    {ok, Req, Env}.
