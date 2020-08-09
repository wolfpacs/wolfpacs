-module(config_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([ test_missing_config/1
	, test_correct_config/1
	, test_broken_config/1
	, test_bad_term_config/1
	]).

all() -> [ test_missing_config
	 , test_correct_config
	 , test_broken_config
	 , test_bad_term_config
	 ].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    {ok, _} = application:ensure_all_started(wolfpacs),
    Cfg.

end_per_suite(Cfg) ->
    ok = application:stop(wolfpacs),
    Cfg.

test_missing_config(Config) ->
    Filename = testutils:read_testfile(Config, "missing.conf"),
    error = wolfpacs_config:load_config_file(Filename).

test_correct_config(Config) ->
    Filename = testutils:read_testfile(Config, "correct.conf"),
    ok = wolfpacs_config:load_config_file(Filename).

test_broken_config(Config) ->
    Filename = testutils:read_testfile(Config, "broken.conf"),
    error = wolfpacs_config:load_config_file(Filename).

test_bad_term_config(Config) ->
    Filename = testutils:read_testfile(Config, "bad_term.conf"),
    error = wolfpacs_config:load_config_file(Filename).
