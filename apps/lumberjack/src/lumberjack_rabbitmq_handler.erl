-module(lumberjack_rabbitmq_handler).
-behaviour(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("amqp_client/include/amqp_client.hrl").


%% Initialization
init([{host, Host}, {port, Port}, {username, User}, {password, Password}, {exchange, Exchange}]) ->
    %% Start RabbitMQ connection
    {ok, Connection} = amqp_connection:start(#amqp_params_network{
        host = Host,
        port = Port,
        username = User,
        password = Password
    }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    
    %% Declare the exchange
    amqp_channel:call(Channel, #'exchange.declare'{
        exchange = Exchange,
        type = <<"direct">>
    }),
    
    %% Return the initial state as a map
    {ok, #{connection => Connection, channel => Channel, exchange => Exchange}}.

%% Handle Logger Events
handle_event({Level, _GL, {Logger, Msg, _Ts, _Md}}, State) ->
    %% Extract RabbitMQ details from the state
    Channel = maps:get(channel, State),
    Exchange = maps:get(exchange, State),
    
    %% Format the log message
    FormattedMsg = io_lib:format("[~p] ~p: ~s", [Level, Logger, Msg]),
    
    %% Publish the message to RabbitMQ
    amqp_channel:cast(Channel, #'basic.publish'{
        exchange = Exchange,
        routing_key = <<"logs">>,
        mandatory = false,
        immediate = false
    }, #amqp_msg{payload = FormattedMsg}),
    
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%% Handle synchronous calls
handle_call(_Request, State) ->
    {ok, not_supported, State}.

%% Handle unexpected messages
handle_info(_Info, State) ->
    {ok, State}.

%% Termination cleanup
terminate(_Reason, State) ->
    %% Extract RabbitMQ details from the state
    Channel = maps:get(channel, State),
    Connection = maps:get(connection, State),
    
    %% Close RabbitMQ resources
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

%% Code changes (for hot code upgrades)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
