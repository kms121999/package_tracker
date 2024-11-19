-module(database_client).
-behavior(gen_server).

%% API
-export([start_link/0, connect/0, put/4, get/3, delete/3, disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(SERVER, ?MODULE).

%% Starts the gen_server process
start_link() ->
    io:format("Starting database_client...~n"),
    lumberjack_server:info("Starting gen_server", #{module => ?MODULE}),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API Functions that make calls to gen_server

connect() ->
    lumberjack_server:info("Connecting to database", #{module => ?MODULE, node => node()}),

    Connection = gen_server:call({?MODULE, 'database@database.keatonsmith.com'}, connect),
    lumberjack_server:info("Connection established to database", #{module => ?MODULE, connection => Connection, node => node()}),
    Connection.

put(Connection, Bucket, Key, Data) ->
    lumberjack_server:info("Sending data to Riak", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),
    gen_server:call({?MODULE, 'database@database.keatonsmith.com'}, {put, Connection, Bucket, Key, Data}).

get(Connection, Bucket, Key) ->
    lumberjack_server:info("Getting data from Riak", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),
    gen_server:call({?MODULE, 'database@database.keatonsmith.com'}, {get, Connection, Bucket, Key}).

delete(Connection, Bucket, Key) ->
    lumberjack_server:info("Deleting data from Riak", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),
    gen_server:call({?MODULE, 'database@database.keatonsmith.com'}, {delete, Connection, Bucket, Key}).

disconnect(Connection) ->
    lumberjack_server:info("Disconnecting from Riak", #{module => ?MODULE, connection => Connection, node => node()}),
    gen_server:call({?MODULE, 'database@database.keatonsmith.com'}, {disconnect, Connection}).

%% gen_server callback implementations

init([]) ->
    io:format("Initializing database_client...~n"),
    lumberjack_server:info("Initializing gen_server", #{module => ?MODULE}),
    {ok, #{}}.

handle_call(connect, _From, State) ->
    lumberjack_server:info("Creating connection to database", #{module => ?MODULE, node => node()}),
    io:format("Connecting to database...~n"), %% Debugging

    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),

    lumberjack_server:info("Connection established", #{module => ?MODULE, connection => Pid, node => node()}),
    io:format("Connected to database with Pid: ~p~n", [Pid]), %% Debugging

    {reply, {ok, Pid}, State};

handle_call({put, Connection, Bucket, Key, Data}, _From, State) ->
    lumberjack_server:info("Saving data to database", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),
    io:format("Saving data to database: Bucket=~p, Key=~p, Data=~p~n", [Bucket, Key, Data]), %% Debugging

    case Connection of
        undefined ->
            lumberjack_server:error("No connection to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection}),
            {reply, {error, no_connection}, State};
        Pid ->
            Object = riakc_obj:new(Bucket, Key, Data),
            case riakc_pb_socket:put(Pid, Object) of
                ok ->
                    {reply, ok, State};
                {error, Reason} ->
                    lumberjack_server:error("Error putting to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection, reason => Reason}),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get, Connection, Bucket, Key}, _From, State) ->
    lumberjack_server:info("Retrieving data from database", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),
    io:format("Getting data from Riak: Bucket=~p, Key=~p~n", [Bucket, Key]), %% Debugging

    case Connection of
        undefined ->
            lumberjack_server:error("No connection to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection}),
            {reply, {error, no_connection}, State};
        Pid ->
            case riakc_pb_socket:get(Pid, Bucket, Key) of
                {ok, Obj} -> {reply, {ok, binary_to_term(riakc_obj:get_value(Obj))}, State};
                {error, Reason} ->
                    lumberjack_server:error("Error getting from database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection, reason => Reason}),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({delete, Connection, Bucket, Key}, _From, State) ->
    lumberjack_server:info("Deleting data from database", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),
    io:format("Deleting data from Riak: Bucket=~p, Key=~p~n", [Bucket, Key]), %% Debugging

    case Connection of
        undefined ->
            lumberjack_server:error("No connection to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection}),
            {reply, {error, no_connection}, State};
        Pid ->
            case riakc_pb_socket:delete(Pid, Bucket, Key) of
                ok -> {reply, ok, State};
                {error, Reason} ->
                    lumberjack_server:error("Error deleting from database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection, reason => Reason}),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({disconnect, Pid}, _From, State) ->
    lumberjack_server:info("Disconnecting from database", #{module => ?MODULE, connection => Pid, node => node()}),
    io:format("Disconnecting connection to database: ~p~n", [Pid]), %% Debugging

    case Pid of
        undefined ->
            io:format("No connection to disconnect.~n"), %% Debugging
            {reply, ok, State};
        Pid ->
            io:format("Disconnecting from Riak: ~p~n", [Pid]), %% Debugging
            riakc_pb_socket:stop(Pid),
            {reply, ok, State}
    end.

handle_cast(Msg, State) ->
    lumberjack_server:warning("Unimplemented method called: handle_cast", #{module => ?MODULE, message => Msg, node => node()}),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Terminating database_client...~n"),
    lumberjack_server:info("Terminating gen_server", #{module => ?MODULE}),
    ok.