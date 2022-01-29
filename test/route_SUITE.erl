-module(route_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_minimal_route/1]).

all() -> [test_minimal_route].

init_per_suite(Cfg) ->
    application:ensure_all_started(wolfpacs),
    Cfg.

end_per_suite(Cfg) ->
    application:stop(wolfpacs),
    Cfg.

test_minimal_route(Config) ->
    %% Add mockup PACS system
    MockName = "mock",
    MockHost = "localhost",
    MockPort = integer_to_list(rand:uniform(1000) + 22222),
    MockAE = "MockAE",
    Self = self(),
    spawn(fun() -> server(MockPort, Self) end),

    wolfpacs_clients:add("mock", "ninja"),
    wolfpacs_dests:add(MockName, MockHost, MockPort, MockAE),
    wolfpacs_workers:add("worker 1", "127.0.0.1", 11113, <<"AE">>),
    wolfpacs_clients:assoc_worker("mock", "worker 1"),
    wolfpacs_clients:assoc_dest("mock", MockName),

    %% Pretend that an dicom file arrives from the outside
    Filename = filename:join([?config(data_dir, Config), "0000.dcm"]),

    timer:sleep(100),
    send_dicom_file_to_outside(Filename),

    receive
	{ok, _Bin} ->
	    ok;
	_ ->
	    ct:fail("Incorrect answer")
    after 5000 ->
	    ct:fail("No message received")
    end.

send_dicom_file_to_outside(Filename) ->
    {ok, StoreSCU} = dcmtk_storescu:start_link(),
    dcmtk_storescu:send(StoreSCU, "localhost", "11112", Filename),
    dcmtk_storescu:stop(StoreSCU).

do_recv(Sock, Listener) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Bin} ->
	    Listener ! {ok, Bin};
	{error, closed} ->
	    Listener ! error
    end.

server(Port, Listener) ->
    {ok, LSock} = gen_tcp:listen(list_to_integer(Port), [binary, {packet, 0}, {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    do_recv(Sock, Listener),
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(LSock).
