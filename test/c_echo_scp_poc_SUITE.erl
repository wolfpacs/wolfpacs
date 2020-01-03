-module(c_echo_scp_poc_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_c_echo_scp_scu/1]).

all() -> [test_c_echo_scp_scu].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

test_c_echo_scp_scu(_Config) ->
    application:ensure_all_started(wolfpacs),
    {ok, Echo} = wolfpacs_c_echo_scu:start_link(),
    {ok, success} = wolfpacs_c_echo_scu:echo(Echo, "localhost", 11112, <<"testtest">>),
    application:stop(wolfpacs).
