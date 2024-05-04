%%%-------------------------------------------------------------------
%% @doc client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(client_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(HOST, {127, 0, 0, 1}).
-define(PORT, 2224).
-define(SERVER, ?MODULE).

start_link(Env) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Env]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Env]) ->
    io:format("~p~n", [Env]),
    Host = proplists:get_value(host, Env, ?HOST),
    Port = proplists:get_value(port, Env, ?PORT),

    {ok, ServerSocket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
    io:format("server socket: ~p~n", [ServerSocket]),

    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => cliente_tcp_child,
		    start => {tcp_client, start_link, [ServerSocket]},
		    modules => [tcp_client]
		   }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
