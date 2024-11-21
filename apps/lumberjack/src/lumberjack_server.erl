%% logger_service.erl
-module(lumberjack_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, add_handler/1, log/3, info/2, warning/2, error/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Starts the logger service
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Stops the logger service
stop() ->
    gen_server:call(?MODULE, stop).

%% Adds a handler to the event manager
add_handler(Handler) ->
    gen_server:call(?MODULE, {add_handler, Handler}).

%% API to log messages with custom level
log(Level, Message, Metadata) ->
    gen_server:cast({?MODULE, 'backend@backend.keatonsmith.com'}, {log, Level, Message, Metadata}).

%% Convenience API to log info messages
info(Message, Metadata) ->
    log(info, Message, Metadata).

%% Convenience API to log warning messages
warning(Message, Metadata) ->
    log(warning, Message, Metadata).

%% Convenience API to log error messages
error(Message, Metadata) ->
    log(error, Message, Metadata).

%% gen_server init callback
init([]) ->
    %% Start the event manager to handle log events
    {ok, EventManager} = gen_event:start_link(),
    gen_event:add_handler(EventManager, lumberjack_console_handler, []),
    gen_event:add_handler(EventManager, lumberjack_file_handler, ["./lumberjack.log"]),
    gen_event:add_handler(EventManager, lumberjack_rabbitmq_handler, [{host, "rabbitmq.keatonsmith.com"}, {port, 5672}, {username, "guest"}, {password, "guest"}, {exchange, "log_exchange"}]),
    {ok, EventManager}.

%% Handle synchronous calls
handle_call({add_handler, Handler}, _From, EventManager) ->
    %% Add the specified handler to the event manager
    case gen_event:add_handler(EventManager, Handler, []) of
        ok -> {reply, ok, EventManager};
        {error, Reason} -> {reply, {error, Reason}, EventManager}
    end;
handle_call(stop, _From, EventManager) ->
    %% Stop the logger service and terminate
    {stop, normal, ok, EventManager}.

%% Handle asynchronous log messages
handle_cast({log, Level, Message, Metadata}, EventManager) ->
    %% Send the log event to all registered handlers
    gen_event:notify(EventManager, {log, Level, Message, Metadata}),
    {noreply, EventManager}.

%% Handle the server termination (clean up)
terminate(_Reason, EventManager) ->
    %% Stop the event manager
    gen_event:stop(EventManager),
    ok.

%% Handle code upgrades (if needed)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
