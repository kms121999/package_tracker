
%% package_get_server.erl
-module(package_get_server).
-behavior(gen_server).

%% API
-export([start_link/0, get_package_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%% Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initialize the state, and connect to database database
init([]) ->
    %% Create the connection to the database database (assuming database_client:connect/0 exists)
    case database_client:connect() of
        {ok, Connection} ->
            io:format("Connected to database database for retrieval with connection: ~p~n", [Connection]),
            {ok, Connection};  %% Pass connection as the initial state
        {error, Reason} ->
            io:format("Failed to connect to database database. Reason: ~p~n", [Reason]),
            {stop, Reason}  %% Stop the gen_server if connection fails
    end.


get_package_data(PackageId) ->
		%% Synchronous call to the gen_server to fetch package data
		gen_server:call(?MODULE, {get_package_data, PackageId}).

%% Handle synchronous calls
handle_call({get_package_data, PackageId}, _From, Connection) ->
    %% Query database for the package
    case database_client:get(Connection, <<"packages">>, PackageId) of
        {ok, Data} ->
            %% Found the package data, return it
            case database_client:get(Connection, <<"trucks">>, maps:get(truckId, Data)) of
                {ok, TruckData} ->
                    {reply, {ok, maps:put(location, TruckData, Data)}, Connection};
                {error, not_found} ->
                    {reply, {ok, maps:put(location, nil, Data)}, Connection};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            %% Handle the case where the package is not found
            io:format("Package ~p not found in database~n", [PackageId]),
            {reply, {error, not_found}, Connection};
        {error, Reason} ->
            %% General error handling
            io:format("Error retrieving package ~p from database. Reason: ~p~n", [PackageId, Reason]),
            {reply, {error, Reason}, Connection}
    end.

%% Handle the server termination (clean up)
terminate(_Reason, Connection) ->
    %% Close the database connection
    database_client:disconnect(Connection),
    ok.

%% Handle code upgrades (if needed)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.







%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% -include_lib("meck/include/meck.hrl").
 




%% Test for package retrieval success and failure using mocked database_client
package_retrieval_test_() ->
    %% Setup test context
    {setup,
        fun setup/0, 
        fun cleanup/1, 
        [
            fun test_package_found/0
        ]
    }.

%% Setup function to mock database_client before each test
setup() ->
    %% Start mocking the database_client module
    meck:new(database_client),
    
    %% Mock the connect function to always succeed
    meck:expect(database_client, connect, 0, {ok, mock_connection}),
    
    %% Mock the disconnect function
    meck:expect(database_client, disconnect, 1, ok),
    
    %% Start the package_get_server service
    {ok, Pid} = package_get_server:start_link(),
    
    %% Return the Pid to use in cleanup
    Pid.

%% Cleanup function to unload the mocks
cleanup(Pid) ->
    %% Stop the package_get_server process
    gen_server:stop(Pid),
    
    %% Unload the meck mock for database_client
    meck:unload(database_client).

test_package_found()->
    StoredPackageData = #{
        sender => "Alice", 
        receiver => "Bob", 
        destination => 
            #{ street => "123 Cat Lane", 
                city => "Wonderland", 
                state => "NY", 
                zip => "12345", 
                country => "USA" }, 
        returnAddress => 
            #{ street => "456 Yellow Brick Rd", 
                city => "OZ", 
                state => "KS", 
                zip => "54321", 
                country => "England" }, 
        status => "in transit", 
        priority => "overnight", 
        truckId => "truck123"
    },

    StoredPackageDataFakeTruck = maps:put(truckId, "faketruck", StoredPackageData),
    FinalPackageDataFakeTruck = maps:put(location, nil, StoredPackageDataFakeTruck),

    StoredPackageDataTruckFail = maps:put(truckId, "truckfail", StoredPackageData),

    FinalPackageData = maps:put(location, #{
        longitude => "-72.532", 
        latitude => "42.532"
    }, StoredPackageData),
    
	 %% Mock the get function to return package data when requested
    meck:expect(database_client, get, 3, 
        fun (_Connection, <<"packages">>, <<"package123">>) ->
                {ok, StoredPackageData};
            (_Connection, <<"trucks">>, <<"truck123">>) ->
                {ok, #{
                    longitude => "-72.532", 
                    latitude => "42.532"
                }};
            (_Connection, <<"packages">>, <<"fakepackage">>) ->
                {error, not_found};
            (_Connection, <<"trucks">>, <<"faketruck">>) ->
                {error, not_found};
            (_Connection, <<"packages">>, <<"package123-faketruck">>) ->
                {ok, StoredPackageDataFakeTruck};
            (_Connection, <<"packages">>, <<"package123-truckfail">>) ->
                {ok, StoredPackageDataTruckFail};
            (_Connection, <<"trucks">>, <<"databasedown">>) ->
                {error, "Database down"};
            (_Connection, <<"packages">>, <<"databasedown">>) ->
                {error, "Database down"}
        end
	),

	% happy thoughts
    ?assertEqual({ok, FinalPackageData}, get_package_data(<<"package123">>)),
	?assertEqual({ok, FinalPackageDataFakeTruck}, get_package_data(<<"package123-fakeTruck">>)),
    % nasty thoughts start here
	?assertEqual({error, not_found}, get_package_data(<<"fakepackage">>)),
    ?assertEqual({error, "Database down"}, get_package_data(<<"package123-truckfail">>)),
	?assertEqual({error, "Database down"}, get_package_data(<<"databasedown">>)).

-endif.