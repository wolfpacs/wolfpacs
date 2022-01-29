-module(file_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_file_meta_information/1,
	 test_little_explicit/1,
	 test_little_implicit/1,
	 test_big_explicit/1]).

all() -> [test_file_meta_information,
	  test_little_explicit,
	  test_little_implicit,
	  test_big_explicit].

init_per_suite(Cfg) ->
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

test_file_meta_information(Config) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    Filename = filename:join([?config(data_dir, Config), "testdata1.bin"]),
    {ok, Content} = file:read_file(Filename),
    {ok, Info, <<>>} = wolfpacs_file_meta_information:decode(Flow, Strategy, Content),

    Get = fun(Group, Element) -> maps:get({Group, Element}, Info, missing) end,

    %% Extract correct information using
    %% $ dcmdump testdata1.bin

    %% (0002,0001) OB 00\01
    [0, 1] = Get(2, 1),

    %% (0002,0002) UI =CTImageStorage
    <<"1.2.840.10008.5.1.4.1.1.2">> = Get(2, 2),

    %% (0002,0003) UI []
    <<"2.16.840.1.113662.2.1.4519.41582.4105152.419990505.410523251">> = Get(2, 3),

    %% (0002,0010) UI =LittleEndianExplicit
    <<"1.2.840.10008.1.2.1">> = Get(2, 16#10),

    %% (0002,0012) UI []
    <<"2.16.840.1.113662.2.1.1">> = Get(2, 16#12),

    %% (0002,0016) AE [PHOENIXSCP]
    <<"PHOENIXSCP">> = Get(2, 16#16),

    %% OK
    ok.

check_file_read_ok(Filename) ->
    logger:warning("FILE: ~p", [Filename]),
    {ok, Flow} = wolfpacs_flow:start_link(),
    {ok, Content} = file:read_file(Filename),
    Strategy = {explicit, little},
    {ok, {_Meta, _Info}, <<>>} = wolfpacs_file_format:decode(Flow, Strategy, Content).

test_little_explicit(Config) ->
    Filename = filename:join([?config(data_dir, Config), "littleexplicit.dcm"]),
    check_file_read_ok(Filename).

test_little_implicit(Config) ->
    Filename = filename:join([?config(data_dir, Config), "littleimplicit.dcm"]),
    check_file_read_ok(Filename).

test_big_explicit(Config) ->
    Filename = filename:join([?config(data_dir, Config), "bigexplicit.dcm"]),
    check_file_read_ok(Filename).
