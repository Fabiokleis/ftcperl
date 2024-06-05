# ftcperl

File Server over tcp written in erlang. 
simple python client.

Protocol
----

![flux(12)](https://github.com/Fabiokleis/ftcperl/assets/66813406/48f22bc2-3bc3-4fc6-8484-0cd3dc1f036a)



Server
----
An OTP application.

Server env options, listening port and process pool size.
```erlang
[
  {server, [{port, 8123}, {pool, 10}]}
].
```

Build
-----

    $ rebar3 compile

Run
----
    $ rebar shell

Client
----
    $ python client.py

Usage
----
```erlang
-define(HELP_MESSAGE, "comande o servidor digitando um dos comandos.

comandos disponiveis: [chat, file, sair, help].
    help: mostra essa mensagem de ajuda.
    sair: termina o processo com o servidor.
    chat: habilita e desabilita o modo chat. %% echo server
    file <nome>: copia um arquivo desejado. %% e.g file client.py
").
```
