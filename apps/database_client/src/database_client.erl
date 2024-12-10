-module(database_client).

%% API
-export([connect/0, put/4, get/3, delete/3, disconnect/1]).

connect() ->
    lumberjack_server:info("Connecting to database", #{module => ?MODULE, node => node()}),

    lumberjack_server:info("Creating connection to database", #{module => ?MODULE, node => node()}),
    io:format("Connecting to database...~n"), %% Debugging

    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),

    lumberjack_server:info("Connection established", #{module => ?MODULE, connection => Pid, node => node()}),
    io:format("Connected to database with Pid: ~p~n", [Pid]), %% Debugging
    {reply, {ok, Pid}}.
    

put(Connection, Bucket, Key, Data) ->
    lumberjack_server:info("Sending data to Riak", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),
    lumberjack_server:info("Saving data to database", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),

    case Connection of
        undefined ->
            lumberjack_server:error("No connection to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection}),
            {reply, {error, no_connection}, Key};
        Pid ->
            Object = riakc_obj:new(Bucket, Key, Data),
            case riakc_pb_socket:put(Pid, Object) of
                ok ->
                    {reply, ok, Key};
                {error, Reason} ->
                    lumberjack_server:error("Error putting to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection, reason => Reason}),
                    {reply, {error, Reason}, Key}
            end
    end.

get(Connection, Bucket, Key) ->
    lumberjack_server:info("Getting data from Riak", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),

    case Connection of
        undefined ->
            lumberjack_server:error("No connection to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection}),
            {reply, {error, no_connection}, Key};
        Pid ->
            case riakc_pb_socket:get(Pid, Bucket, Key) of
                {ok, Obj} -> {reply, {ok, binary_to_term(riakc_obj:get_value(Obj))}, Key};
                {error, Reason} ->
                    lumberjack_server:error("Error getting from database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection, reason => Reason}),
                    {reply, {error, Reason}, Key}
            end
    end.

delete(Connection, Bucket, Key) ->
    lumberjack_server:info("Deleting data from Riak", #{module => ?MODULE, bucket => Bucket, key => Key, node => node()}),

    case Connection of
        undefined ->
            lumberjack_server:error("No connection to database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection}),
            {reply, {error, no_connection}, Key};
        Pid ->
            case riakc_pb_socket:delete(Pid, Bucket, Key) of
                ok -> {reply, ok, Key};
                {error, Reason} ->
                    lumberjack_server:error("Error deleting from database", #{module => ?MODULE, node => node(), bucket => Bucket, key => Key, connection => Connection, reason => Reason}),
                    {reply, {error, Reason}, Key}
            end
    end.

disconnect(Connection) ->
    lumberjack_server:info("Disconnecting from Riak", #{module => ?MODULE, connection => Connection, node => node()}),
    io:format("Disconnecting connection to database: ~p~n", [Connection]), %% Debugging

    case Connection of
        undefined ->
            io:format("No connection to disconnect.~n"), %% Debugging
            {reply, ok};
        Pid ->
            io:format("Disconnecting from Riak: ~p~n", [Connection]), %% Debugging
            riakc_pb_socket:stop(Pid),
            {reply, ok}
    end.