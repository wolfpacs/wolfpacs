%%%-------------------------------------------------------------------
%% @doc C Echo SCP.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_c_echo_scp).
-export([encode/3]).

-define(CMD, 16#0000).
-define(UID, 16#0002).
-define(FLD, 16#0100).

-define(RQID, 16#0110).
-define(RPID, 16#0120).
-define(SET, 16#0800).
-define(STU, 16#0900).

encode(Strategy, UID, RQID) ->
    Info = #{{?CMD, ?UID, "UI"} => UID,
	     {?CMD, ?FLD, "US"} => 16#8030,
	     {?CMD, ?RPID, "US"} => RQID,
	     {?CMD, ?SET, "US"} => 16#0101,
	     {?CMD, ?STU, "US"} => 16#0000},
    Data = wolfpacs_data_elements:encode(Strategy, Info),
    NbBytes = byte_size(Data),
    lager:warning("[c_echo_scp] encode length ~p", [NbBytes]),
    Header = wolfpacs_data_element:encode(Strategy, 0, 0, "UL", NbBytes),
    <<Header/binary, Data/binary>>.

%%==============================================================================
%% Private
%%==============================================================================


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    RQID = 1,
    UID = <<"1.2.3.4">>,
    Correct = <<0, 0, 0, 0,
		4, 0, 0, 0,
		56, 0, 0, 0,
		0,
		0, 2, 0, 8, 0,
		0, 0,
		49, 46, 50, 46, 51, 46, 52, 0,
		0, 0, 0, 1, 2, 0, 0, 0,
		48, 128, 0, 0, 32, 1, 2,
		0, 0, 0, 1, 0, 0, 0, 0, 8, 2,
		0, 0, 0, 1, 1, 0, 0, 0, 9, 2, 0, 0, 0, 0, 0>>,
    [ ?_assertEqual(encode({implicit, little}, UID, RQID), Correct)
    , ?_assertEqual(encode({explicit, little}, UID, RQID), Correct)
    ].
