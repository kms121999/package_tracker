
-module(lumberjack_rabbitmq_handler).
-behaviour(gen_event).

%% API
-export([init/1, handle_event/2, handle_info/2, terminate/2, code_change/3]).

