-module(database_client).
-behavior(gen_server).

%% API
-export([start_link/0, connect/0, put/4, get/3, delete/3, disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2]).

-define(SERVER, ?MODULE).

%% Starts the gen_server process
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API Functions that make calls to gen_server

connect() ->
    gen_server:call({?MODULE, 'riak@riak.keatonsmith.com'}, connect).

put(Connection, Bucket, Key, Data) ->
    gen_server:call({?MODULE, 'riak@riak.keatonsmith.com'}, {put, Connection, Bucket, Key, Data}).

get(Connection, Bucket, Key) ->
    gen_server:call({?MODULE, 'riak@riak.keatonsmith.com'}, {get, Connection, Bucket, Key}).

delete(Connection, Bucket, Key) ->
    gen_server:call({?MODULE, 'riak@riak.keatonsmith.com'}, {delete, Connection, Bucket, Key}).

disconnect(Connection) ->
    gen_server:call({?MODULE, 'riak@riak.keatonsmith.com'}, {disconnect, Connection}).

%% gen_server callback implementations

init([]) ->
    io:format("Starting database_client...~n"),
    {ok, #{}}.

handle_call(connect, _From, State) ->
    io:format("Connecting to Riak database...~n"),
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    io:format("Connection established: ~p~n", [Pid]),
    {reply, {ok, Pid}, maps:put(connection, Pid, State)};

handle_call({put, Connection, Bucket, Key, Data}, _From, State) ->
    io:format("Saving data to Riak: Bucket=~p, Key=~p, Data=~p~n", [Bucket, Key, Data]),
    case maps:get(connection, Connection, undefined) of
        undefined ->
            {reply, {error, no_connection}, State};
        Pid ->
            Object = riakc_obj:new(Bucket, Key, Data),
            case riakc_pb_socket:put(Pid, Object) of
                ok -> {reply, ok, State};
                {error, Reason} -> {reply, {error, Reason}, State}
            end
    end;

handle_call({get, Connection, Bucket, Key}, _From, State) ->
    io:format("Getting data from Riak: Bucket=~p, Key=~p~n", [Bucket, Key]),
    case maps:get(connection, Connection, undefined) of
        undefined ->
            {reply, {error, no_connection}, State};
        Pid ->
            case riakc_pb_socket:get(Pid, Bucket, Key) of
                {ok, Obj} -> {reply, {ok, riakc_obj:get_value(Obj)}, State};
                {error, Reason} -> {reply, {error, Reason}, State}
            end
    end;

handle_call({delete, Connection, Bucket, Key}, _From, State) ->
    io:format("Deleting data from Riak: Bucket=~p, Key=~p~n", [Bucket, Key]),
    case maps:get(connection, Connection, undefined) of
        undefined ->
            {reply, {error, no_connection}, State};
        Pid ->
            case riakc_pb_socket:delete(Pid, Bucket, Key) of
                ok -> {reply, ok, State};
                {error, Reason} -> {reply, {error, Reason}, State}
            end
    end;

handle_call({disconnect, Connection}, _From, State) ->
    case maps:get(connection, Connection, undefined) of
        undefined ->
            io:format("No connection to disconnect.~n"),
            {reply, ok, State};
        Pid ->
            io:format("Disconnecting from Riak: ~p~n", [Pid]),
            riakc_pb_socket:stop(Pid),
            {reply, ok, maps:remove(connection, State)}
    end.

terminate(_Reason, _State) ->
    io:format("Terminating database_client...~n"),
    ok.