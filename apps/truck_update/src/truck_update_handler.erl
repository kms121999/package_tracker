%% truck_handler.erl
-module(truck_update_handler).
-export([init/2, terminate/3]).
-behaviour(cowboy_handler).



init(Req=#{method := <<"POST">>}, State) ->

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    io:format("DATA: ~p~n", [Body]),
    ParsedData = jiffy:decode(Body, [return_maps]),
    io:format("DATA: ~p~n", [ParsedData]),


    %% Extract values
    TruckID = maps:get(<<"truckId">>, ParsedData),
    Lat = maps:get(<<"lat">>, maps:get(<<"location">>, ParsedData)),
    Long = maps:get(<<"long">>, maps:get(<<"location">>, ParsedData)),

    %% Call the truck_update server
    Result = truck_update_server:update_location(TruckID, Lat, Long),

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
