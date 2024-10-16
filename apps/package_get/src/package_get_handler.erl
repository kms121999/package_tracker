%% package_get_handler.erl
-module(package_get_handler).
-behaviour(cowboy_http).

%% Required exports
-export([init/2, handle/2, terminate/3]).

%% Include libraries (for JSON encoding, if needed)
-include_lib("jsx/include/jsx.hrl").  %% Assuming using jsx for JSON

%% Initialize the HTTP handler
init(Req, State) ->
    {cowboy_rest, Req, State}.

%% Handle GET requests for full package data
handle(Req, State) ->
    %% Parse the package ID (or truck ID) from the request path
    {ok, PackageId, Req1} = cowboy_req:binding(package_id, Req),

    %% Retrieve the full package data using truck_data_retriever:get_package_data/1
    case package_get_server:get_package_data(PackageId) of
        {ok, PackageData} ->
            %% Create the JSON response from PackageData
            ResponseJson = jsx:encode(PackageData),
            %% Send 200 OK response with the package data in JSON
            {ok, Req2} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseJson, Req1),
            {ok, Req2, State};

        {error, not_found} ->
            %% Handle the case where the package was not found
            ErrorJson = jsx:encode(#{<<"error">> => <<"Package not found">>}),
            {ok, Req2} = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorJson, Req1),
            {ok, Req2, State};

        {error, Reason} ->
            %% Handle any other errors
            ErrorJson = jsx:encode(#{<<"error">> => <<"Error retrieving package data">>,
                                     <<"reason">> => Reason}),
            {ok, Req2} = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorJson, Req1),
            {ok, Req2, State}
    end.

%% Terminate callback (not used in this case)
terminate(_Req, _State, _Reason) ->
    ok.
