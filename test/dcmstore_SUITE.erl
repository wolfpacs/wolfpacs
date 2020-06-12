-module(dcmstore_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([compatibility_with_storescu/1]).

all() -> [compatibility_with_storescu].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

compatibility_with_storescu(Config) ->
    application:ensure_all_started(wolfpacs),

    {ok, Flow} = wolfpacs_flow:start_link(),

    Filename = filename:join([?config(data_dir, Config), "0000.dcm"]),
    Dataset1 = dataset_from_file(Flow, Filename),
    Dataset2 = dataset_using_dcmtk_storescu(Flow, Filename),

    F = fun(DS) -> lists:sort(maps:to_list(DS)) end,

    compare(F(Dataset1), F(Dataset2)).

dataset_from_file(Flow, Filename) ->
    {ok, Content} = file:read_file(Filename),
    Strategy = {explicit, little},
    {ok, {_Meta, Info}, <<>>} = wolfpacs_file_format:decode(Flow, Strategy, Content),
    Info.

dataset_using_dcmtk_storescu(_Flow, Filename) ->
    {ok, U} = dcmtk_storescu:start_link(),
    dcmtk_storescu:send(U, "localhost", "11112", Filename),

    {ok, Stored} = wolfpacs_storage:retreive(),
    dcmtk_storescu:stop(U),

    Stored.

compare([], []) ->
    ok;
compare([H|T1], [H|T2]) ->
    compare(T1, T2);
compare([H1|_], [H2|_]) ->
    lager:warning("H1 ~p", [H1]),
    lager:warning("H2 ~p", [H2]),
    %% fail test
    H1 = H2.
