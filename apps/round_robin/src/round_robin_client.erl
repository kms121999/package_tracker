-module(round_robin_client).
-behavior(gen_server).

%% API Functions
-export([start_link/1, next_node/0, add_node/1, remove_node/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% State Record
-record(state, {
    nodes = [],   %% List of backend nodes (e.g., {IP, Port})
    index = 0     %% Current round-robin index
}).

%% API: Start the GenServer
start_link(Nodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Nodes, []).

%% API: Get the Next Node
next_node() ->
    gen_server:call({?MODULE, 'roundrobin@roundrobin.keatonsmith.com'}, get_next).

%% API: Add a Node
add_node(Node) ->
    gen_server:cast({?MODULE, 'roundrobin@roundrobin.keatonsmith.com'}, {add_node, Node}).

%% API: Remove a Node
remove_node(Node) ->
    gen_server:cast({?MODULE, 'roundrobin@roundrobin.keatonsmith.com'}, {remove_node, Node}).

%% Initialization
init(Nodes) ->
    {ok, #state{nodes = Nodes}}.

%% Handle Calls
handle_call(get_next, _From, #state{nodes = Nodes, index = Index} = State) ->
    case Nodes of
        [] -> {reply, {error, no_nodes}, State};
        _ ->
            Node = lists:nth(Index + 1, Nodes),
            NewIndex = (Index + 1) rem length(Nodes),
            {reply, {ok, Node}, State#state{index = NewIndex}}
    end.

%% Handle Casts for Add/Remove
handle_cast({add_node, Node}, #state{nodes = Nodes} = State) ->
    case lists:member(Node, Nodes) of
        true -> {noreply, State};
        false -> {noreply, State#state{nodes = Nodes ++ [Node]}}
    end;
handle_cast({remove_node, Node}, #state{nodes = Nodes} = State) ->
    NewNodes = lists:delete(Node, Nodes),
    {noreply, State#state{nodes = NewNodes}}.

%% Cleanup and Version Changes
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
