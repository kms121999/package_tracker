-module(req_id_middleware).
-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    %% Generate a unique request ID
    ReqId = generate_request_id(),
    %% Add the request ID to the request environment
    Req1 = maps:put(package_tracker, #{req_id => ReqId}, Req),

    %% Pass the modified request to the next middleware or handler
    {ok, Req1, Env}.

%% Helper function to generate a unique request ID.
generate_request_id() ->
    %% Using UUIDs here, but you could also use a timestamp, counter, etc.
    uuid:uuid_to_string(uuid:get_v4()).
