%% package_update.erl
-module(package_update_server).
-behavior(gen_server).

-export([start_link/0, update_location/3]).

-export([init/1, handle_call/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_location(packageID, Lat, Long) ->
    gen_server:call(?MODULE, {update, packageID, Lat, Long}).

init([]) ->
    {ok, #{}}.

handle_call({update, PackageID, Lat, Long}, _From, State) ->
    %% Simulate interaction with db_client here
    case db_client:find_package(PackageID) of
        {ok, Package} ->
            UpdatedPackage = Package#{latitude => Lat, longitude => Long},
            ok = db_client:update_package(UpdatedPackage),
            {reply, {ok, updated}, State};
        error ->
            NewPackage = #{id => PackageID, latitude => Lat, longitude => Long},
            ok = db_client:package(NewPackage),
            {reply, {ok, inserted}, State}
    end.

terminate(_Reason, Connection) ->
    %% Close the database connection
    database_client:disconnect(Connection),
    ok.