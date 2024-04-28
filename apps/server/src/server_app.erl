%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Env = application:get_all_env(server),
    server_sup:start_link(Env).

stop(_State) ->
    ok.

%% internal functions
