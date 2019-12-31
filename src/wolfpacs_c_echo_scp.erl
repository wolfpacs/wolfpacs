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
