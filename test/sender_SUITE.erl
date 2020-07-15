-module(sender_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_send_mr/1]).

all() -> [test_send_mr].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

test_send_mr(Config) ->
    Filename = filename:join([?config(data_dir, Config), "0000.dcm"]),
    DataSet = testutils:read_dataset(Filename),
    Host = "127.0.0.1",
    Port = 1234,
    CalledAE = <<"CalledAE">>,
    CallingAE = <<"CallingAE">>,
    {ok, Sender} = wolfpacs_sender:start_link(Host, Port, CalledAE, CallingAE),
    wolfpacs_sender:send(Sender, DataSet),
    wolfpacs_sender:stop(Sender).
