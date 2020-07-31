-module(sq_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([ test_sq_explicit/1
	, test_sq_implicit/1
	]).

all() -> [ test_sq_explicit
	 , test_sq_implicit
	 ].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

test_sq_common(Filename) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    {ok, Content} = file:read_file(Filename),
    {ok, {Meta, Info}, <<>>} = wolfpacs_file_format:decode(Flow, Strategy, Content),

    GetMeta = fun(Group, Element) ->
		      maps:get({Group, Element}, Meta, missing)
	      end,
    GetInfo = fun(Group, Element) ->
		      maps:get({Group, Element}, Info, missing)
	      end,

    %% Extract correct information using
    %% $ dcmdump

    [0, 1] = GetMeta(2, 1),

    <<"1.2.840.10008.5.1.4.1.1.4">> = GetMeta(2, 2),

    <<"1.2.826.0.1.3680043.8.1055.1.20111103111201790.69919593.65388240">> = GetMeta(2, 3),

    <<"1.2.276.0.7230010.3.0.3.6.4">> = GetMeta(2, 16#12),

    <<"ORIGINAL\\PRIMARY\\OTHER">> = GetInfo(0008,0008),

    <<"20070101">> = GetInfo(16#0008, 16#0012),

    CorrectSQ = [#{{8,256} => <<"121327">>,
		   {8,258} => <<"DCM">>,
		   {8,260} => <<"Full fidelity image, uncompressed or lossless compressed">>}],

    CorrectSQ = GetInfo(16#0008, 16#9215).

test_sq_explicit(Config) ->
    Filename = filename:join([?config(data_dir, Config), "example_one.dcm"]),
    test_sq_common(Filename).

test_sq_implicit(Config) ->
    Filename = filename:join([?config(data_dir, Config), "example_two.dcm"]),
    test_sq_common(Filename).
