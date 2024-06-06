%%%-------------------------------------------------------------------
%% @doc tcp_server public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server).

-behaviour(gen_server).
 
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
code_change/3, terminate/2]).

 -include_lib("kernel/include/file.hrl").

-define(HELP_MESSAGE, "comande o servidor digitando um dos comandos.

comandos disponiveis: [chat, file, sair, help].
    help: mostra essa mensagem de ajuda.
    sair: termina o processo com o servidor.
    chat: habilita e desabilita o modo chat. %% echo server
    file <nome>: copia um arquivo desejado. %% e.g file client.py
").

-define(CHUNK_SIZE, 8192).

%% server supported commands
-type command() :: parse | exit | chat | help | unknown.

%% tcp socket connection state
-record(state, {socket :: inet:socket(),
		client_id :: string() | none(),
		command :: command()}).

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

handle_cast(accept, S = #state{socket=ListenSocket, client_id=none}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket), %% get received tcp socket
    server_sup:start_socket(), %% a new acceptor is born, praise the lord
    {noreply, S#state{socket=AcceptSocket, client_id=gen_client_id(8), command=parse}}. 

send(Socket, Str, _Args) ->
    ok = gen_tcp:send(Socket, Str),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

-spec calc_checksum(Fd, CheckSum, ChunkSize) -> binary() | {error, atom()} when 
      Fd :: file:io_device(),
      CheckSum :: crypto:hash_state(),
      ChunkSize :: pos_integer(). 
calc_checksum(Fd, Checksum, ChunkSize) ->
    case file:read(Fd, ChunkSize) of
        {ok, Data} ->
            NewChecksum = crypto:hash_update(Checksum, Data),
            calc_checksum(Fd, NewChecksum, ChunkSize);
        eof -> crypto:hash_final(Checksum);
        {error, Reason} -> {error, Reason}
    end.

-spec parse_message(Message :: string()) -> command() | {file, string()}.
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
    case file:open(Path, [read, raw, binary]) of
	{ok, Fd} -> {Fd, calc_checksum(Fd, crypto:hash_init(sha256), ?CHUNK_SIZE)};
	{error, Reason} -> {error, Reason}
    end.

send_file(Socket, File, Acc) ->
    case file:pread(File, Acc, ?CHUNK_SIZE) of
	{ok, Chunk} ->
	    send(Socket, Chunk, []), 
	    send_file(Socket, File, Acc + ?CHUNK_SIZE);
	Any -> Any
    end.

send_file(Socket, File) ->
    send_file(Socket, File, 0).
    
handle_info({tcp, Socket, Msg}, S = #state{client_id=ClientId, command=parse}) ->
    io:format("client_id=~p message=~p~n", [ClientId, Msg]),
    case parse_message(Msg) of
	exit -> {stop, normal, S};
	unknown -> send(Socket, "comando desconhecido, digite help!\n", []), {noreply, S};
	chat -> send(Socket, "modo de chat habilitado\n", []), {noreply, S#state{command=chat}};
	help -> send(Socket, ?HELP_MESSAGE, []), {noreply, S};
	client_id -> send(Socket, ClientId, []), {noreply, S};
	{file, Path} -> 
	    case check_file(strip(Path)) of
		{error, Reason} -> 
		    send(Socket, io_lib:format("failed to open file: ~p, cause: ~p\n", [Path, Reason]), []),
		    {noreply, S};
		{File, CheckSum} -> 
		    %% send(Socket, "Not implemented yet.\n", []), 
		    {ok, FileInfo} = file:read_file_info(File),
		    _Size = FileInfo#file_info.size,
		    Type = FileInfo#file_info.type,

		    io:format("client_id=~p check_sum=~p~n", [ClientId, CheckSum]),
		    send(Socket, binary_to_list(CheckSum), []), %% send checksum

		    case Type of
			regular -> 
			    case send_file(Socket, File) of
				eof -> file:close(File), {stop, normal, S};
				{error, Reason} -> 
				    io:format("failed to read file chunk, reason: ~p", [Reason]), 
				    {stop, normal, S}
			    end;
			_ -> 
			    send(Socket, io:format("tipo de arquivo ~p, nao suportado\n", [Type]), []),
			    {stop, normal, S}
		    end
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
