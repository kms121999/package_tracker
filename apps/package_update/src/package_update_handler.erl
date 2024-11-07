%% package_update_handler.erl
-module(package_update_handler).
-export([init/2, terminate/3]).
-behaviour(cowboy_handler).



init(Req=#{method := <<"POST">>}, State) ->

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ParsedData = jiffy:decode(Body, [return_maps]),

    %% Extract values
    PackageID = maps:get(<<"packageID">>, ParsedData),
    Data = maps:remove(PackageID, ParsedData),
    
    %% Call the package_update server
    Result = package_update_server:update_package(PackageID, Data),

    %% Prepare and send response
    Response = case Result of
        {ok, Status} -> {200, #{status => Status}};
        {error, Reason} -> {500, #{error => Reason}}
    end,
    {StatusCode, RespBody} = Response,
    Req2 = cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(RespBody), Req1),
    {ok, Req2, State};


init(Req0, State) ->
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.




 

