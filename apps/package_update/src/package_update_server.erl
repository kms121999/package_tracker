%% package_update.erl
-module(package_update_server).
-behavior(gen_server).

-export([start_link/0, update_package/2]).

-export([init/1, handle_call/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_package(PackageID, Package_data) ->
    gen_server:call({?MODULE, 'backend@backend.keatonsmith.com'}, {update, PackageID, Package_data}).

init([]) ->
    lumberjack_server:info("Initializing gen_server", #{module => ?MODULE}),
    %% Create the connection to the database database (assuming database_client:connect/0 exists)
    case database_client:connect() of
        {ok, Connection} ->
            lumberjack_server:info("Connected to database", #{module => ?MODULE, connection => Connection}),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            lumberjack_server:error("Failed to connect to database", #{module => ?MODULE, reason => Reason}),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.

handle_call({update, PackageID, Package_data}, _From, State) ->
    %% Simulate interaction with db_client here
    case database_client:get(State, <<"packages">>, PackageID) of
        {ok, _Data} ->
            
            case database_client:put(State, <<"packages">>, PackageID, Package_data) of
                ok ->
                    {reply, {ok, replaced}, State};
                {error, _Reason} ->
                    {reply, {error, database_error}, State}
                end;
            
        {error, notfound} ->
            ok = database_client:put(State, <<"packages">>, PackageID, Package_data),
            {reply, {ok, inserted}, State}
    end.

handle_cast(Msg, Connection) ->
    lumberjack_server:warning("Unimplemented method called: handle_cast", #{module => ?MODULE, message => Msg}),
    {noreply, Connection}.

terminate(_Reason, Connection) ->
    %% Close the database connection
    database_client:disconnect(Connection),
    ok.


%% Import EUnit and Meck for testing and mocking
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

package_update_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun test_package_found/0
        ]
    }.

setup() ->
    meck:new(database_client),

    meck:expect(database_client, connect, 0, {ok, mock_connection}),
    
    meck:expect(database_client, disconnect, 1, ok),

        %% Start the package_get_server service
        {ok, Pid} = package_update_server:start_link(),
    
        %% Return the Pid to use in cleanup
        Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    
    %% Unload the meck mock for database_client
    meck:unload(database_client).


test_package_found()->
    Package123 = <<"package123">>,
    StoredPackageData = #{
        <<"sender">> => <<"Alice">>, 
        <<"receiver">> => <<"Bob">>, 
        <<"destination">> => 
            #{ <<"street">> => <<"123 Cat Lane">>, 
                <<"city">> => <<"Wonderland">>, 
                <<"state">> => <<"NY">>, 
                <<"zip">> => <<"12345">>, 
                <<"country">> => <<"USA">> }, 
        <<"returnAddress">> => 
            #{ <<"street">> => <<"456 Yellow Brick Rd">>, 
                <<"city">> => <<"OZ">>, 
                <<"state">> => <<"KS">>, 
                <<"zip">> => <<"54321">>, 
                <<"country">> => <<"England">> }, 
        <<"status">> => <<"in transit">>, 
        <<"priority">> => <<"overnight">>, 
        <<"truckId">> => <<"truck123">>
    },

    meck:expect(database_client, get, 3, 
        fun (_Connection, <<"packages">>, <<"package123">>) ->
                ok;
            (_Connection, <<"packages">>, <<"bad_package">>) ->
                {error, notfound};
            (_Connection, <<"packages">>, <<"databasedown">>) ->
                {error, "Database down"}

        end
	),

    meck:expect(database_client, put, 4, 
        fun (_Connection, <<"packages">>, <<"package123">>, StoredData) ->
                ok;
            (_Connection, <<"packages">>, <<"bad_package">>, StoredData) ->
                {error, StoredPackageData};
            (_Connection, <<"packages">>, <<"databasedown">>, StoredData) ->
                {error, "Database down"}

        end
	),

    ?assertEqual(ok, package_update_server:update_package(Package123, StoredPackageData)).

-endif.