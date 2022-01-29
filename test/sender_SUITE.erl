-module(sender_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("wolfpacs_types.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_send_mr/1]).

all() -> [test_send_mr].

init_per_suite(Cfg) ->
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

test_send_mr(Config) ->
    Filename = filename:join([?config(data_dir, Config), "0000.dcm"]),
    DataSet = testutils:read_dataset(Filename),
    Remote = #wolfpacs_remote{ host = "127.0.0.1"
			     , port = 1234
			     , ae = <<"CalledAE">>},
    {ok, Sender} = wolfpacs_sender:start_link(Remote),
    wolfpacs_sender:send(Sender, DataSet),
    wolfpacs_sender:stop(Sender).
