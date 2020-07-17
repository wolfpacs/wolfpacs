-module(dcmstore_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([ receive_mr_image/1
	, receive_presentation_state/1
	]).

all() -> [ receive_mr_image
	 , receive_presentation_state
	 ].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    application:ensure_all_started(wolfpacs),
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

receive_mr_image(Config) ->
    send_receive_test_file(Config, "0000.dcm").

receive_presentation_state(Config) ->
    send_receive_test_file(Config, "gsps.dcm").

send_receive_test_file(Config, TestFile) ->
    {ok, Flow} = wolfpacs_flow:start_link(),

    wolfpacs_storage:empty(),
    Filename = filename:join([?config(data_dir, Config), TestFile]),
    Dataset1 = testutils:read_dataset(Flow, Filename),
    Dataset2 = dataset_using_dcmtk_storescu(Flow, Filename),

    compare_length(Dataset1, Dataset2),

    F = fun(DS) -> lists:sort(maps:to_list(DS)) end,

    compare(F(Dataset1), F(Dataset2)).

dataset_using_dcmtk_storescu(_Flow, Filename) ->
    {ok, U} = dcmtk_storescu:start_link(),
    timer:sleep(1000),
    dcmtk_storescu:send(U, "localhost", "11112", Filename),

    ct:sleep(500),
    {ok, Stored} = wolfpacs_storage:retreive(),
    dcmtk_storescu:stop(U),

    Stored.

compare([], []) ->
    ok;
compare([H|T1], [H|T2]) ->
    compare(T1, T2);
compare([{Header, Value1}|_], [{Header, Value2}|_]) ->
    ct:fail("Header is correct, value differ ~p ~p", [Value1, Value2]);
compare([{H1, _}|_], [{H2, _}|_]) ->
    ct:fail("Headers differ ~p ~p", [H1, H2]).

compare_length(M1, M2) ->
    L1 = maps:size(M1),
    L2 = maps:size(M2),
    case L1 == L2 of
	true ->
	    ok;
	false ->
	    ct:fail("Lengths differ, ~p != ~p", [L1, L2])
    end.
