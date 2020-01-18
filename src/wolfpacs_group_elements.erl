%%%-------------------------------------------------------------------
%% @doc Group Elements.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_group_elements).
-export([vr/2]).

-spec vr(integer(), integer()) -> list().
vr(Group, Element) ->
    DB = wolfpacs_group_elements_db:db(),
    vr_lookup(Group, Element, maps:get({Group, Element}, DB, missing)).

vr_lookup(Group, Element, missing) ->
    lager:warning("[group_elements] Unknown (~p, ~p)", [Group, Element]),
    "UN";
vr_lookup(_, _, VR) ->
    VR.

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

vr_found_test_() ->
    [ ?_assertEqual(vr(?CMD, ?UID), "UI"),
      ?_assertEqual(vr(?CMD, ?FLD), "US"),
      ?_assertEqual(vr(?CMD, ?RPID), "US"),
      ?_assertEqual(vr(?CMD, ?SET), "US"),
      ?_assertEqual(vr(?CMD, ?STU), "US") ].
