%% package_update_handler.erl
-module(package_update_handler).
-export([init/2, terminate/3]).
-behaviour(cowboy_handler).



init(Req=#{method := <<"POST">>}, State) ->

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ParsedData = jiffy:decode(Body, [return_maps]),

    %% Extract values
    PackageID = maps:get(<<"packageID">>, ParsedData),
    Lat = maps:get(<<"lat">>, maps:get(<<"loc">>, ParsedData)),
    Long = maps:get(<<"long">>, maps:get(<<"loc">>, ParsedData)),

    %% Call the package_update server
    Result = package_update:update_location(package_D, Lat, Long),

    %% Prepare and send response
    Response = case Result of
        {ok, Status} -> {200, #{status => Status}};
        {error, Reason} -> {500, #{error => Reason}}
    end,
    {StatusCode, RespBody} = Response,
    {ok, Req2} = cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(RespBody), Req1),
    {ok, Req2, State};


init(Req0, State) ->
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.



%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 

-endif.