-module(c_echo_scp_poc_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test_connection/1]).

all() -> [test_connection].

test_connection(_Config) ->
    application:ensure_all_started(wolfpacs),
    {ok, Sock} = gen_tcp:connect("localhost", 11112, [binary]),
    ok = gen_tcp:send(Sock, test_data_echo:c_echo_scu()),
    ok = gen_tcp:close(Sock).
