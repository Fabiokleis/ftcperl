%%%-------------------------------------------------------------------
%% @doc tcp_server public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_client).

-export([client/1]).
-export([client_loop/0, server_loop/2, input_stdin/1, check_input/1]).

client_loop() ->
    receive
	{_From, {filechunk, _Data}} -> 
	    io:format("Not implemented yet!~n"), 
	    client_loop();
	_ -> 
	    client_loop()
    end.

server_loop(Socket, Pid) ->
    receive
	{_From, input, exit} ->
	    case gen_tcp:send(Socket, [<<"sair\n\r">>]) of
		ok -> gen_tcp:close(Socket);
		_ -> server_loop(Socket, Pid)
	    end;
	{_From, input, Data} ->
	    gen_tcp:send(Socket, [Data]),
	    server_loop(Socket, Pid);
        {Client, send_data, Binary} ->
            case gen_tcp:send(Socket,[Binary]) of
                {error, timeout} ->
                    io:format("Send timeout, closing!~n",
                              []),
                    %% handle_send_timeout(), % Not implemented here
                    Client ! {self(),{error_sending, timeout}},
                    %% Usually, it's a good idea to give up in case of a 
                    %% send timeout, as you never know how much actually 
                    %% reached the server, maybe only a packet header?!
                    gen_tcp:close(Socket);
                {error, OtherSendError} ->
                    io:format("Some other error on socket (~p), closing",
                              [OtherSendError]),
                    Client ! {self(),{error_sending, OtherSendError}},
                    gen_tcp:close(Socket);
                ok ->
                    Client ! {self(), data_sent},
                    server_loop(Socket, Pid)
            end
    end.

check_input(<<"sair\n">> = Input) -> Input;
check_input(Input) -> Input.
   
input_stdin(Pid) ->
    Input = list_to_bitstring(io:get_line(standard_io, "enter>")),
    io:format("input: ~p~n", [Input]),
    case check_input(Input) of
	<<"sair\n">> -> Pid ! {self(), input, exit};
	_ ->
	    Pid ! {self(), input, Input},
	    input_stdin(Pid)
    end.

client(Socket) ->
    ClientPid = spawn_link(?MODULE, client_loop, []),    
    ServerPid = spawn_link(?MODULE, server_loop, [Socket, ClientPid]),
    input_stdin(ServerPid).
    %%spawn_link(?MODULE, input_stdin, [ServerPid]).
 
