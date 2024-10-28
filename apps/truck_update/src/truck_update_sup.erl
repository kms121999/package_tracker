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
    %% Define the child specification for Cowboy
    Dispatch = cowboy_router:compile([
                {'_', [{"/truck/:truck_id", truck_update_handler, []}]}
    % {"packages.localhost", [{"/:package_id", package_get_handler, []}]}
    ]),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => cowboy_http_listener,
            start => {cowboy, start_clear, [http_listener, [{port, 8081}], #{
                env => #{dispatch => Dispatch}
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cowboy]
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
