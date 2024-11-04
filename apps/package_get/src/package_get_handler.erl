%% package_get_handler.erl
-module(package_get_handler).
-behavior(cowboy_handler).

%% Required exports
-export([init/2, terminate/3]).

%% Include libraries (for JSON encoding, if needed)
% -include_lib("jsx/include/jsx.hrl").  %% Assuming using jsx for JSON

%% Initialize the HTTP handler


%% Handle GET requests for full package data
init(Req0=#{method := <<"GET">>}, State) ->
    %% Parse the package ID (or truck ID) from the request path
    PackageId = cowboy_req:binding(package_id, Req0),
    lumberjack_server:info("Received package get request", #{module => ?MODULE, packageId => PackageId, peer_ip => cowboy_req:peer(Req0)}),

    %% Retrieve the full package data using truck_data_retriever:get_package_data/1
    case package_get_server:get_package_data(PackageId) of
        {ok, PackageData} ->
            lumberjack_server:info("Package data retrieved", #{module => ?MODULE, packageId => PackageId}),
            %% Create the JSON response from PackageData
            ResponseJson = jsx:encode(PackageData),
            %% Send 200 OK response with the package data in JSON
            {ok, Req1} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseJson, Req0),
            {ok, Req1, State};

        {error, not_found} ->
            lumberjack_server:warning("Package not found", #{module => ?MODULE, packageId => PackageId}),
            %% Handle the case where the package was not found
            ErrorJson = jsx:encode(#{<<"error">> => <<"Package not found">>}),
            {ok, Req1} = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorJson, Req0),
            {ok, Req1, State};

        {error, Reason} ->
            lumberjack_server:error("Error retrieving package data", #{module => ?MODULE, reason => Reason, packageId => PackageId}),
            %% Handle any other errors
            ErrorJson = jsx:encode(#{<<"error">> => <<"Error retrieving package data">>,
                                     <<"reason">> => Reason}),
            {ok, Req1} = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorJson, Req0),
            {ok, Req1, State}
    end;

init(Req0, State) ->
    lumberjack_server:warning("Invalid request method", #{module => ?MODULE, method => Req0.method, peer_ip => cowboy_req:peer(Req0)}),
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req0),
    {ok, Req1, State}.

%% Terminate callback (not used in this case)
terminate(_Req, _State, _Reason) ->
    ok.
