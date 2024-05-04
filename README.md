# ftcperl

File Server over tcp written in erlang.

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
    $ telnet localhost 8123
