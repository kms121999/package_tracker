
-module(lumberjack_file_handler).
-behaviour(gen_event).

%% API
-export([init/1, handle_event/2, handle_info/2, terminate/2, code_change/3]).

%% Initialize the handler and open the log file
init([FilePath]) ->
    case file:open(FilePath, [append, write]) of
        {ok, File} ->
            {ok, #{file => File}};
        {error, Reason} ->
            io:format("Failed to open log file: ~p~n", [Reason]),
            {stop, Reason}
    end.

%% Handle incoming log events and write them to the file
handle_event({log, Level, Message, Metadata}, State = #{file := File}) ->
    Timestamp = os:timestamp(),
    LogEntry = io_lib:format("~p - [~p] ~p: ~p~n", [Timestamp, Level, Message, Metadata]),
    file:write(File, LogEntry),
    {ok, State}.

%% Handle other messages (unused)
handle_info(_Info, State) ->
    {ok, State}.

%% Terminate the handler and close the log file
terminate(_Reason, _State = #{file := File}) ->
    file:close(File),
    ok.

%% Handle code upgrades (unused)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
