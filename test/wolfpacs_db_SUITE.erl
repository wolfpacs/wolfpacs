-module(wolfpacs_db_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([all/0]).
-export([add_workers/1, add_clients/1]).

all() ->
    [add_workers, add_clients].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    application:stop(mnesia),
    wolfpacs_db:uninstall([node()]),
    wolfpacs_db:install([node()]),
    application:start(mnesia),
    application:start(wolfpacs),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    wolfpacs_db:uninstall([node()]),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

add_workers(_Config) ->
    ok = wolfpacs_db:add_worker("my-worker-1", "192.168.1.11", 1111, "W1AE"),
    ok = wolfpacs_db:add_worker("my-worker-2", "192.168.1.22", 2222, "W2AE").

add_clients(_Config) ->
    ok = wolfpacs_db:add_client("my-client-1", "SecretString"),
    ok = wolfpacs_db:add_client("my-client-2", "SuperSecret").
