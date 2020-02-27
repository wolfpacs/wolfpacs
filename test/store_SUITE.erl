-module(store_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_store_ct/1,
	 test_store_mr/1]).

all() -> [test_store_ct,
	  test_store_mr].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    application:ensure_all_started(wolfpacs),
    Cfg.

end_per_suite(Cfg) ->
    application:stop(wolfpacs),
    Cfg.

test_store_ct(Config) ->
    Filename = filename:join([?config(data_dir, Config), "ct.dcm"]),
    dcmsend(Filename).

test_store_mr(Config) ->
    Filename = filename:join([?config(data_dir, Config), "mr.dcm"]),
    dcmsend(Filename).

dcmsend(Filename) ->
    {ok, S} = wolfpacs_dcmsend_port:start_link("localhost", "11112"),
    {ok, exit_status, 0} = wolfpacs_dcmsend_port:send(S, Filename),
    wolfpacs_dcmsend_port:stop(S).
