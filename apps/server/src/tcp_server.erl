%%%-------------------------------------------------------------------
%% @doc tcp_server public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server).

-behaviour(gen_server).
 
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
code_change/3, terminate/2]).

-define(HELP_MESSAGE, "cliente: comande o servidor digitando um dos comandos.

comandos disponiveis: [chat, file, sair, help].
    help: mostra essa mensagem de ajuda.
    sair: termina o processo com o servidor.
    chat: habilita e desabilita o modo chat.
    file <nome>: copia um arquivo desejado.
").

%% server supported commands
-type command() :: parse | exit | chat | help | unknown | {file, string()}.

%% tcp socket connection state
-record(state, {socket :: inet:socket(), client_id :: string() | none(), command :: command()}).

gen_client_id(Len) ->
    base64:encode_to_string(crypto:strong_rand_bytes(Len)).
 
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).
 
init(Socket) ->
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept), %% call handle_cast(accept...)
    {ok, #state{socket=Socket, client_id=none, command=parse}}.
 
%% We never need you, handle_call!
handle_call(_E, _From, State) ->
    {noreply, State}.

send(Socket, Str, _Args) ->
    ok = gen_tcp:send(Socket, Str),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

-spec parse_message(Message :: string()) -> command().
parse_message(<<"sair\r\n">>) -> exit;
parse_message(<<"chat\r\n">>) -> chat;			     
parse_message(<<"help\r\n">>) -> help;
parse_message(<<"client_id\r\n">>) -> client_id;
parse_message(<<"file ", Path/bitstring>>) -> {file, Path};
parse_message(_) -> unknown.

strip(Path) ->
    Size = bit_size(Path) div 8 - 2,
    <<Name:Size/binary, "\r\n">> = Path,
    Name.

-spec check_file(Path :: string()) -> {file:io_device(), string()} | {error, atom()}.
check_file(Path) ->
    case file:open(Path, [raw]) of
	{ok, File} -> {File, "this should be a checksum"};
	{error, Reason} -> {error, Reason}
    end.
    
handle_cast(accept, S = #state{socket=ListenSocket, client_id=none}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket), %% get received tcp socket
    server_sup:start_socket(), %% a new acceptor is born, praise the lord
    {noreply, S#state{socket=AcceptSocket, client_id=gen_client_id(8), command=parse}}.

handle_info({tcp, Socket, Msg}, S = #state{client_id=ClientId, command=parse}) ->
    io:format("received message: ~p~n", [Msg]),
    case parse_message(Msg) of
	exit -> send(Socket, "saindo...\n", []), {stop, normal, S};
	unknown -> send(Socket, "comando desconhecido, digite help!\n", []), {noreply, S};
	chat -> send(Socket, "modo de chat habilitado\n", []), {noreply, S#state{command=chat}};
	help -> send(Socket, ?HELP_MESSAGE, []), {noreply, S};
	client_id -> send(Socket, ClientId ++ "0", []), {noreply, S};
	{file, Path} -> 
	    case check_file(strip(Path)) of
		{error, Reason} -> 
		    send(Socket, io_lib:format("failed to open file: ~p, cause: ~p\n", [Path, Reason]), []),
		    {noreply, S};
		{_File, CheckSum} -> 
		    %% send(Socket, "Not implemented yet.\n", []), 
		    send(Socket, CheckSum ++ "2", []),
		    {noreply, S}
	    end
    end;

%% only stops sending echo if chat was typed again
handle_info({tcp, Socket, Msg}, S = #state{command=chat}) ->
    io:format("received message: ~p~n", [Msg]),
    case parse_message(Msg) of 
	chat -> send(Socket, "modo de chat desabilitado\n", []), {noreply, S#state{command=parse}};
	_ -> send(Socket, Msg, []), {noreply, S}  %% echo tcp server
    end;

handle_info({tcp_closed, _Socket}, S) ->
    io:format("~p: closed socket~n", [S]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _ = #state{socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    ok;

terminate(Reason, State = #state{socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    io:format("~p: terminate reason: ~p~n", [State, Reason]),
    ok.
