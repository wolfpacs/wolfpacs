-module(c_echo_scp_poc_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_c_echo_scp_scu/1,
	 test_storescu_associate_rq/1]).

all() -> [test_c_echo_scp_scu,
	  test_storescu_associate_rq].

init_per_suite(Cfg) ->
    {ok, _} = application:ensure_all_started(wolfpacs),
    Cfg.

end_per_suite(Cfg) ->
    ok = application:stop(wolfpacs),
    Cfg.

test_c_echo_scp_scu(_Config) ->
    {ok, Echo} = wolfpacs_c_echo_scu:start_link(),
    {ok, success} = wolfpacs_c_echo_scu:echo(Echo, "localhost", 11112, <<"testtest">>, {implicit, little}),
    {ok, success} = wolfpacs_c_echo_scu:echo(Echo, "localhost", 11112, <<"testtest">>, {explicit, little}),
    {ok, success} = wolfpacs_c_echo_scu:echo(Echo, "localhost", 11112, <<"testtest">>, {explicit, big}).

test_storescu_associate_rq(Config) ->
    Filename = filename:join([?config(data_dir, Config), "storescu_associate_rq.bin"]),
    {ok, VariableItems} = file:read_file(Filename),
    {ok, _Contexts, _MaxSize, _Class, _VersionName, <<>>} = wolfpacs_variable_items_request:decode(VariableItems).
