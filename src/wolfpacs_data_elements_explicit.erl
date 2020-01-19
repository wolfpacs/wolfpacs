%%%-------------------------------------------------------------------
%% @doc Data Elements (Explicit Little).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_elements_explicit).
-export([encode_map/1,
	 encode_list/1,
	 decode/1]).

-spec encode_map(map()) -> binary().
encode_map(Info) ->
    encode_list(maps:to_list(Info)).

-spec encode_list(list(any())) -> binary().
encode_list(Elements) ->
    encode(lists:sort(Elements), <<>>).

-spec decode(binary()) -> {ok, map(), binary()} | {error, binary()}.
decode(Data) ->
    decode(Data, []).

%%==============================================================================
%% Private Encoders
%%==============================================================================

encode([], Acc) ->
    Acc;
encode([{{G, E}, Data}|Rest], Acc) ->
    VR = wolfpacs_group_elements:vr(G, E),
    Encoded = wolfpacs_data_element_explicit:encode(G, E, VR, Data),
    encode(Rest, <<Acc/binary, Encoded/binary>>).

-spec decode(binary(), list()) -> {ok, map(), binary()} | {error, binary()}.
decode(<<>>, Acc) ->
    {ok, maps:from_list(Acc), <<>>};
decode(Data, []) ->
    case wolfpacs_data_element_explicit:decode(Data) of
	{error, _} ->
	    {error, Data};
	{ok, Res, Rest} ->
	    decode(Rest, [Res])
    end;
decode(Data, Acc) ->
    case wolfpacs_data_element_explicit:decode(Data) of
	{error, _} ->
	    {ok, maps:from_list(Acc), Data};
	{ok, Res, Rest} ->
	    decode(Rest, [Res|Acc])
    end.

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

encode_list_test() ->
    UID = <<"1.2.3.4">>,
    Items = [{{?CMD, ?UID}, UID}],
    Correct = wolfpacs_data_element_explicit:encode(?CMD, ?UID, "UI", UID),
    ?assertEqual(encode_list(Items), Correct).

encode_decode_list_test() ->
    UID = <<"1.2.3.4">>,
    Items = [{{?CMD, ?UID}, UID},
	     {{?CMD, ?FLD}, 16#8030},
	     {{?CMD, ?RPID}, ?RQID},
	     {{?CMD, ?SET}, 16#0101},
	     {{?CMD, ?STU}, 16#0000}],
    Correct = maps:from_list(Items),
    Encoded0 = encode_list(Items),
    ?assertEqual(decode(Encoded0), {ok, Correct, <<>>}).

encode_decode_map_test() ->
    UID = <<"1.2.3.4">>,
    Items = #{{?CMD, ?UID} => UID,
	      {?CMD, ?FLD} => 16#8030,
	      {?CMD, ?RPID} => ?RQID,
	      {?CMD, ?SET} => 16#0101,
	      {?CMD, ?STU} => 16#0000},
    Encoded0 = encode_map(Items),
    ?assertEqual(decode(Encoded0), {ok, Items, <<>>}).

encode_decode_empty_map_test() ->
    Items = #{},
    Encoded0 = encode_map(Items),
    ?assertEqual(decode(Encoded0), {ok, Items, <<>>}).
