%% logger_console_handler.erl
-module(lumberjack_console_handler).
-behaviour(gen_event).

%% API
-export([init/1, handle_event/2, handle_info/2, terminate/2, code_change/3]).

%% Initialize the handler
init([]) ->
    {ok, #{}}.

%% Handle incoming log events
handle_event({log, Level, Message, Metadata}, State) ->
    io:format("~p - [~p] ~p: ~p~n", [os:timestamp(), Level, Message, Metadata]),
    {ok, State}.

%% Handle other messages (unused)
handle_info(_Info, State) ->
    {ok, State}.

%% Terminate the handler
terminate(_Reason, _State) ->
    ok.

%% Handle code upgrades (unused)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
