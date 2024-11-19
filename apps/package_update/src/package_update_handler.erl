%% package_update_handler.erl
-module(package_update_handler).
-export([init/2, terminate/3]).
-behaviour(cowboy_handler).



init(Req=#{method := <<"POST">>}, State) ->
    Req_id = maps:get(req_id, maps:get(package_tracker, Req)),

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ParsedData = jiffy:decode(Body, [return_maps]),

    %% Extract values
    PackageId = maps:get(<<"packageId">>, ParsedData),
    Data = maps:remove(PackageId, ParsedData),

    lumberjack_server:info("Received package update request", #{module => ?MODULE, packageId => PackageId, peer_ip => cowboy_req:peer(Req1), req_id => Req_id}),

    
    %% Call the package_update server
    Result = package_update_server:update_package(PackageId, Data),

    %% Prepare and send response
    Response = case Result of
        {ok, Status} ->
            lumberjack_server:info("Package updated", #{module => ?MODULE, packageId => PackageId, req_id => Req_id, status => Status}),
            {200, #{status => Status}};
        {error, Reason} ->
            lumberjack_server:error("Package update failed", #{module => ?MODULE, packageId => PackageId, req_id => Req_id, reason => Reason}),
            {500, #{error => Reason}}
    end,
    {StatusCode, RespBody} = Response,
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




 

