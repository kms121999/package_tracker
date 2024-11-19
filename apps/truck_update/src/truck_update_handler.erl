%% truck_handler.erl
-module(truck_update_handler).
-export([init/2, terminate/3]).
-behaviour(cowboy_handler).



init(Req=#{method := <<"POST">>}, State) ->
    Req_id = maps:get(req_id, maps:get(package_tracker, Req)),

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ParsedData = jiffy:decode(Body, [return_maps]),

    %% Extract values
    TruckId = maps:get(<<"truckId">>, ParsedData),
    Lat = maps:get(<<"lat">>, maps:get(<<"location">>, ParsedData)),
    Long = maps:get(<<"long">>, maps:get(<<"location">>, ParsedData)),

    lumberjack_server:info("Received truck update request", #{module => ?MODULE, truckId => TruckId, peer_ip => cowboy_req:peer(Req1), req_id => Req_id}),

    %% Call the truck_update server
    truck_update_server:update_location(TruckId, Lat, Long, Req_id),

    lumberjack_server:info("Truck location update triggered", #{module => ?MODULE, truckId => TruckId, req_id => Req_id}),

    %% Prepare and send response
    {StatusCode, RespBody} = {202, #{status => noreply}},
    Req2 = cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(RespBody), Req1),
    {ok, Req2, State};


init(Req0, State) ->
    Req_id = maps:get(req_id, maps:get(package_tracker, Req0)),

    lumberjack_server:warning("Invalid request method", #{module => ?MODULE, method => cowboy_req:method(Req0), peer_ip => cowboy_req:peer(Req0), req_id => Req_id}),
    
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
