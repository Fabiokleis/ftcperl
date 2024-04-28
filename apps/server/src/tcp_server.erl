%%%-------------------------------------------------------------------
%% @doc tcp_server public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server).

-behaviour(gen_server).
 
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
code_change/3, terminate/2]).

-record(session, {socket, state}).
 
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).
 
init(Socket) ->
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept), %% call handle_cast(accept...)
    {ok, #session{socket=Socket}}.
 
%% We never need you, handle_call!
handle_call(_E, _From, State) ->
    {noreply, State}.

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

handle_cast(accept, S = #session{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket), %% get received tcp socket
    server_sup:start_socket(), % a new acceptor is born, praise the lord
    {noreply, S#session{socket=AcceptSocket, state=read}}.

handle_info({tcp, Socket, Msg}, S = #session{state=read}) ->
    send(Socket, Msg, []), %% echo tcp server
    {noreply, S};
    
handle_info({tcp_closed, _Socket}, S) ->
    io:format("~p: closed socket~n", [S]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(Reason, State) ->
    io:format("~p: terminate reason: ~p~n", [State, Reason]).
