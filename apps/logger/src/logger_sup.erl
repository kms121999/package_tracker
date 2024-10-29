%%%-------------------------------------------------------------------
%% @doc package_tracker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(package_get_sup).

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
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        % #{id => cowboy_http_listener,
        %     start => {cowboy, start_clear, [http_listener, [{port, 8080}], #{
        %         env => #{dispatch => Dispatch}
        %     }]},
        %     restart => permanent,
        %     shutdown => 5000,
        %     type => worker,
        %     modules => [cowboy]
        % },
        #{id => package_get_server,
          start => {package_get_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [package_get_server]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.


%% internal functions
