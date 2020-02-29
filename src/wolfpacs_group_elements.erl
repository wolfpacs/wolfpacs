%%%-------------------------------------------------------------------
%% @doc Group Elements.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_group_elements).
-export([vr/2]).
-export([vr_to_example_group_element/1]).

-spec vr(integer(), integer()) -> list().
vr(Group, Element) ->
    DB = wolfpacs_group_elements_db:db(),
    vr_lookup(Group, Element, maps:get({Group, Element}, DB, missing)).

vr_lookup(Group, Element, missing) ->
    _ = lager:warning("[group_elements] Unknown (~p, ~p)", [Group, Element]),
    "UN";
vr_lookup(_, _, VR) ->
    VR.

%%-------------------------------------------------------------------
%% @doc Return an example {Group, Element} for a VR.
%% Useful when writing implicit test cases.
%%
%% WARNING, Don't pick example in group 0000 because they
%% are always treated as little endian implicit.
%%
%% @end
%%-------------------------------------------------------------------
vr_to_example_group_element("OB") -> {16#0002, 16#0001};
vr_to_example_group_element("OW") -> {16#0028, 16#1201};
vr_to_example_group_element("OF") -> {16#0064, 16#0009};
vr_to_example_group_element("PN") -> {16#0008, 16#0090};
vr_to_example_group_element("AE") -> {16#0040, 16#0001};
vr_to_example_group_element("UI") -> {16#0040, 16#0554};
vr_to_example_group_element("US") -> {16#0040, 16#A0B0};
vr_to_example_group_element("UL") -> {16#0040, 16#A132};
vr_to_example_group_element("LO") -> {16#0008, 16#0070};
vr_to_example_group_element("UN") -> {16#0072, 16#006D};
vr_to_example_group_element("CS") -> {16#0072, 16#0208};
vr_to_example_group_element("DA") -> {16#0008, 16#0012};
vr_to_example_group_element(VR) -> {error, VR, ["No example found"]}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

-define(CMD, 16#0000).
-define(UID, 16#0002).
-define(FLD, 16#0100).

-define(RQID, 16#0110).
-define(RPID, 16#0120).
-define(SET, 16#0800).
-define(STU, 16#0900).

vr_missing_test() ->
    ?assertEqual(vr(16#FFFF, 16#EEEE), "UN").

vr_example_missing_test() ->
    ?assertEqual(vr_to_example_group_element("QQ"), {error, "QQ", ["No example found"]}).

vr_found_test_() ->
    [ ?_assertEqual(vr(?CMD, ?UID), "UI"),
      ?_assertEqual(vr(?CMD, ?FLD), "US"),
      ?_assertEqual(vr(?CMD, ?RPID), "US"),
      ?_assertEqual(vr(?CMD, ?SET), "US"),
      ?_assertEqual(vr(?CMD, ?STU), "US") ].
