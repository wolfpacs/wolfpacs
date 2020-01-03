-module(wolfpacs_c_echo_scp).
-export([react/1]).

-define(CMD, 16#0000).
-define(UID, 16#0002).
-define(FLD, 16#0100).

-define(RQID, 16#0110).
-define(RPID, 16#0120).
-define(SET, 16#0800).
-define(STU, 16#0900).

react({ok, MessageFields}) ->
    react(MessageFields);
react(MessageFields) ->
    Info = maps:from_list([{E, V} || {_, E, _, V} <- MessageFields]),
    react_with_info(Info).

react_with_info(#{?UID := UID, ?RQID := RQID}) ->
    Items = [{?CMD, ?UID, ui, UID},
	     {?CMD, ?FLD, us, 16#8030},
	     {?CMD, ?RPID, us, RQID},
	     {?CMD, ?SET, us, 16#0101},
	     {?CMD, ?STU, us, 16#0000}],
    wolfpacs_dimse_protocol:attach_header(
      wolfpacs_dimse_protocol:encode(Items)).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

react_test() ->
    UID = <<"1.2.3.4">>,
    MessageFields = [{?CMD, ?UID, byte_size(UID), UID},
		     {?CMD, ?RQID, 4, 1}],

    Correct = <<0, 0, 0, 0, 4,
		0, 0, 0, 56,
		0, 0, 0, 0,
		0, 2, 0, 8, 0,
		0, 0,
		49, 46, 50, 46, 51, 46, 52, 32,
		0, 0, 0, 1, 2, 0, 0, 0,
		48, 128, 0, 0, 32, 1, 2,
		0, 0, 0, 1, 0, 0, 0, 0, 8, 2,
		0, 0, 0, 1, 1, 0, 0, 0, 9, 2, 0, 0, 0, 0, 0>>,
    ?assertEqual(react({ok, MessageFields}), Correct).
