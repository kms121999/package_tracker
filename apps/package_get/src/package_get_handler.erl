%% package_get_handler.erl
-module(package_get_handler).

%% Required exports
-export([init/2, terminate/3]).

%% Include libraries (for JSON encoding, if needed)
% -include_lib("jsx/include/jsx.hrl").  %% Assuming using jsx for JSON

%% Initialize the HTTP handler


%% Handle GET requests for full package data
init(Req0=#{method := <<"GET">>}, State) ->
    %% Parse the package ID (or truck ID) from the request path
    PackageId = cowboy_req:binding(package_id, Req0),
    io:format("Got this~p~n", [PackageId]),
    io:format("From HOST: ~p~n", [cowboy_req:host(Req0)]),

    %% Retrieve the full package data using truck_data_retriever:get_package_data/1
    case package_get_server:get_package_data(PackageId) of
        {ok, PackageData} ->
            %% Create the JSON response from PackageData
            ResponseJson = jsx:encode(PackageData),
            %% Send 200 OK response with the package data in JSON
            {ok, Req1} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseJson, Req0),
            {ok, Req1, State};

        {error, not_found} ->
            %% Handle the case where the package was not found
            ErrorJson = jsx:encode(#{<<"error">> => <<"Package not found">>}),
            {ok, Req1} = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorJson, Req0),
            {ok, Req1, State};

        {error, Reason} ->
            %% Handle any other errors
            ErrorJson = jsx:encode(#{<<"error">> => <<"Error retrieving package data">>,
                                     <<"reason">> => Reason}),
            {ok, Req1} = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorJson, Req0),
            {ok, Req1, State}
    end;

init(Req0, State) ->
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req0),
    {ok, Req1, State}.

%% Terminate callback (not used in this case)
terminate(_Req, _State, _Reason) ->
    ok.
