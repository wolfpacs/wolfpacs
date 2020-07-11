%%%-------------------------------------------------------------------
%% @doc Group Elements.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_group_elements).
-export([vr/2]).
-export([vr_to_example_group_element/1]).

-spec vr(integer(), integer()) -> list().
vr(_Group, 0) ->
    %% PS 3.5, 3.6, 7.2 Group Length
    %%  The Group Length (gggg,0000) Standard Data Element
    %%  shall be implicitly defined for all Data Element
    %%  groups with a Value Representation of UL and a Value
    %%  Multiplicity of 1
    "UL";
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
%% Look in wolfpacs_group_elements_db for inspiration.
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
vr_to_example_group_element("AT") -> {16#3008, 16#0062};
vr_to_example_group_element("SH") -> {16#0008, 16#0094};
vr_to_example_group_element("TM") -> {16#0008, 16#0013};
vr_to_example_group_element("DT") -> {16#0008, 16#0015};
vr_to_example_group_element("ST") -> {16#0008, 16#0081};
vr_to_example_group_element("IS") -> {16#0008, 16#1160};
vr_to_example_group_element("AS") -> {16#0010, 16#1010};
vr_to_example_group_element("DS") -> {16#0010, 16#1020};
vr_to_example_group_element("SS") -> {16#0018, 16#9219};
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
