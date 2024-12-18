-module(round_robin_sup).
-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    %% Define the child specification for Cowboy
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => round_robin_client,
          start => {round_robin_client, start_link, [
            ['backend.keatonsmith.com',
            'backend2.keatonsmith.com']
            ]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [round_robin_client]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.


%% internal functions
