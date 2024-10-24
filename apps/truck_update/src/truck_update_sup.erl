%%%-------------------------------------------------------------------
%% @doc truck_update supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(truck_update_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => truck_update_handler,                         %% Identifying the child
          start => {truck_update_handler, start_link, []},    %% Start the child with start_link/0
          restart => permanent,                               %% Restart strategy
          shutdown => 5000,                                   %% Shutdown timeout in ms
          type => worker,                                     %% Declaring it a worker process
          modules => [truck_update_handler]                   %% Modules that define this process
   },
        #{id => truck_update_server,
          start => {truck_update_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [truck_update_server]
    }
],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
