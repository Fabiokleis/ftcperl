%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

%%-compile([{nowarn_unused_function, [{socket_accept_pool,1}]}]).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1, start_socket/0, socket_accept_pool/1]).

-define(PORT, 2224).
-define(POOL, 10).

start_link(Env) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Env]).

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
    Port = proplists:get_value(port, Env, ?PORT),
    Pool = proplists:get_value(pool, Env, ?POOL),

    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}]),

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 60,
                 period => 3600},

    %% start accept listener pool processes to handle multiple connections at the same time
    spawn_link(?MODULE, socket_accept_pool, [Pool]), 
    %% pass tcp listener to gen_server child process
    ChildSpecs = [#{id => server_tcp_child,
		   start => {tcp_server, start_link, [ListenSocket]}, 
		   modules => [tcp_server]
		  }], 
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
start_socket() ->
    supervisor:start_child(?MODULE, []).

socket_accept_pool(Pool) ->
    [start_socket() || _ <- lists:seq(1, Pool)].
