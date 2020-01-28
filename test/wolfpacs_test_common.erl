-module(wolfpacs_test_common).
-export([setup/0,
	 send_garbage/3]).

setup() ->
    application:ensure_all_started(wolfpacs).

send_garbage(Garbage, Hostname, Port) ->
    spawn(fun() -> trasher(Garbage, Hostname, Port) end).

trasher(Garbage, Hostname, Port) ->
    {ok, Sock} = gen_tcp:connect(Hostname, Port, [binary, {active, false}]),
    ok = gen_tcp:send(Sock, Garbage),
    timer:sleep(1000).
