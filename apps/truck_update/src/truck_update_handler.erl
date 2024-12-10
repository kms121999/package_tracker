%% truck_handler.erl
-module(truck_update_handler).
-export([init/2, terminate/3]).
-behaviour(cowboy_handler).



init(Req=#{method := <<"POST">>}, State) ->
    ReqId = 123, %maps:get(req_id, maps:get(package_tracker, Req)),

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ParsedData = jiffy:decode(Body, [return_maps]),

    % lumberjack_server:info("Received truck update request", #{module => ?MODULE, truck_id => TruckId, peer_ip => cowboy_req:peer(Req1), req_id => ReqId}),

    %% Call the truck_update server
    truck_update_server:update_location(ParsedData, ReqId),

    % lumberjack_server:info("Truck location update triggered", #{module => ?MODULE, truck_id => TruckId, req_id => ReqId}),

    %% Prepare and send response
    Req2 = cowboy_req:reply(202, #{}, Req1),
    {ok, Req2, State};


init(Req0, State) ->
    ReqId = maps:get(req_id, maps:get(package_tracker, Req0)),

    lumberjack_server:warning("Invalid request method", #{module => ?MODULE, method => cowboy_req:method(Req0), peer_ip => cowboy_req:peer(Req0), req_id => ReqId}),
    
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
