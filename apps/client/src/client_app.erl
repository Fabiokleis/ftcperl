%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Env = application:get_all_env(client),
    client_sup:start_link(Env).

stop(_State) ->
    ok.

%% internal functions
