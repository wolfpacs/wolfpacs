-module(wolfpacs_test_common).
-export([setup/0,
	 reset/0,
	 send_garbage/3]).

setup() ->
    application:ensure_all_started(wolfpacs).

reset() ->
    wolfpacs_clients:reset(),
    wolfpacs_workers:reset(),
    wolfpacs_dests:reset().

send_garbage(Garbage, Hostname, Port) ->
    spawn(fun() -> trasher(Garbage, Hostname, Port) end).

trasher(Garbage, Hostname, Port) ->
    {ok, Sock} = gen_tcp:connect(Hostname, Port, [binary, {active, false}]),
    ok = gen_tcp:send(Sock, Garbage),
    timer:sleep(1000).
