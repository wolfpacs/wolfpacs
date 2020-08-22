-module(rest_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("wolfpacs_types.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([ test_setup/1
	, test_workers_resource/1
	, test_clients_resource/1
	, test_dests_resource/1
	, test_client_workers/1
	, test_client_dests/1
	]).

all() -> [ test_setup
	 , test_workers_resource
	 , test_clients_resource
	 , test_dests_resource
	 , test_client_workers
	 , test_client_dests
	 ].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    application:ensure_all_started(wolfpacs),
    init_clients(),
    init_workers(),
    init_dests(),
    init_client_workers(),
    init_client_dests(),
    Cfg.

init_clients() ->
    N1 = <<"C1">>,
    C1 = #{<<"name">> => N1,
	   <<"ae">> => <<"C_AE1">>},
    #{<<"name">> := N1} = post_client(C1),

    N2 = <<"C2">>,
    C2 = #{<<"name">> => N2,
	   <<"ae">> => <<"C_AE2">>},
    #{<<"name">> := N2} = post_client(C2),

    N3 = <<"C3">>,
    C3 = #{<<"name">> => N3,
	   <<"ae">> => <<"C_AE3">>},
    #{<<"name">> := N3} = post_client(C3).


init_workers() ->
    Name1 = <<"W1">>,
    W1 = #{<<"name">> => Name1,
	   <<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"W_AE1">>},
    #{<<"name">> := Name1} = post_worker(W1),

    Name2 = <<"W2">>,
    W2 = #{<<"name">> => Name2,
	   <<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"W_AE2">>},
    #{<<"name">> := Name2} = post_worker(W2),

    Name3 = <<"W3">>,
    W3 = #{<<"name">> => Name3,
	   <<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"W_AE3">>},
    #{<<"name">> := Name3} = post_worker(W3).

init_dests() ->
    Name1 = <<"D1">>,
    D1 = #{<<"name">> => Name1,
	   <<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"D_AE1">>},
    #{<<"name">> := Name1} = post_dest(D1),

    Name2 = <<"D2">>,
    D2 = #{<<"name">> => Name2,
	   <<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"D_AE2">>},
    #{<<"name">> := Name2} = post_dest(D2),

    Name3 = <<"D3">>,
    D3 = #{<<"name">> => Name3,
	   <<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"D_AE3">>},
    #{<<"name">> := Name3} = post_dest(D3).

init_client_workers() ->
    post_client_worker("C1", #{<<"name">> => <<"W1">>}),

    post_client_worker("C2", #{<<"name">> => <<"W1">>}),
    post_client_worker("C2", #{<<"name">> => <<"W2">>}),

    post_client_worker("C3", #{<<"name">> => <<"W1">>}),
    post_client_worker("C3", #{<<"name">> => <<"W2">>}),
    post_client_worker("C3", #{<<"name">> => <<"W3">>}).

init_client_dests() ->
    post_client_dest("C1", #{<<"name">> => <<"D1">>}),
    post_client_dest("C2", #{<<"name">> => <<"D2">>}),
    post_client_dest("C3", #{<<"name">> => <<"D3">>}).

end_per_suite(Cfg) ->
    application:stop(wolfpacs),
    Cfg.

get_clients() ->
    testutils:get_resource("/clients").

get_workers() ->
    testutils:get_resource("/workers").

get_dests() ->
    testutils:get_resource("/dests").

get_client_workers(Client) ->
    testutils:get_client_resource("/workers", Client).

get_client_dest(Client) ->
    testutils:get_client_resource("/dest", Client).

post_client(Client) ->
    testutils:post_resource("/clients", Client).

post_worker(Worker) ->
    testutils:post_resource("/workers", Worker).

post_dest(Dest) ->
    testutils:post_resource("/dests", Dest).

post_client_worker(Client, Worker) ->
    testutils:post_client_resource("/workers", Client, Worker).

post_client_dest(Client, Dest) ->
    testutils:post_client_resource("/dest", Client, Dest).

%%                              _            _
%%                             | |_ ___  ___| |_ ___
%%                             | __/ _ \/ __| __/ __|
%%                             | ||  __/\__ \ |_\__ \
%%                              \__\___||___/\__|___/
%%

test_setup(_Config) ->
    {ok, Clients} = wolfpacs_clients:all(),
    3 = length(Clients),

    {ok, Workers} = wolfpacs_workers:all(),
    3 = length(Workers),

    {ok, Dests} = wolfpacs_dests:all(),
    3 = length(Dests),

    {ok, C1Workers} = wolfpacs_clients:workers_for_name("C1"),
    1 = length(C1Workers),
    ok.

test_clients_resource(_Config) ->
    Clients = get_clients(),
    3 = length(Clients).

test_workers_resource(_Config) ->
    Workers = get_workers(),
    3 = length(Workers).

test_dests_resource(_Config) ->
    Dests = get_dests(),
    3 = length(Dests).

test_client_workers(_Config) ->
    [<<"W1">>] = get_client_workers("C1"),
    [<<"W1">>, <<"W2">>] = get_client_workers("C2"),
    [<<"W1">>, <<"W2">>, <<"W3">>] = get_client_workers("C3").

test_client_dests(_Config) ->
    <<"D1">> = get_client_dest("C1"),
    <<"D2">> = get_client_dest("C2"),
    <<"D3">> = get_client_dest("C3").
