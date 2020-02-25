%%%-------------------------------------------------------------------
%% @doc Data Elements (Explicit Little).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_elements).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(strategy(), map() | list()) -> binary().
encode(Strategy, Info) when is_map(Info) ->
    encode(Strategy, maps:to_list(Info));
encode(Strategy, Elements) ->
    encode(Strategy, lists:sort(Elements), <<>>).

-spec decode(strategy(), binary()) -> {ok, map(), binary()} | {error, binary(), list(string())}.
decode(Strategy, Data) ->
    decode(Strategy, Data, []).

%%==============================================================================
%% Private Encoders
%%==============================================================================

encode(_, [], Acc) ->
    Acc;
encode(Strategy, [{{G, E}, Data}|Rest], Acc) ->
    VR = wolfpacs_group_elements:vr(G, E),
    Encoded = wolfpacs_data_element:encode(Strategy, G, E, VR, Data),
    encode(Strategy, Rest, <<Acc/binary, Encoded/binary>>);
encode(Strategy, [{{G, E, VR}, Data}|Rest], Acc) ->
    Encoded = wolfpacs_data_element:encode(Strategy, G, E, VR, Data),
    encode(Strategy, Rest, <<Acc/binary, Encoded/binary>>).

-spec decode(strategy(), binary(), list()) -> {ok, map(), binary()} | {error, binary(), list(string())}.
decode(_, <<>>, []) ->
    {error, <<>>, ["no data to decode"]};
decode(_, <<>>, Acc) ->
    {ok, maps:from_list(Acc), <<>>};
decode(Strategy, Data, []) ->
    case wolfpacs_data_element:decode(Strategy, Data) of
	{error, _, Msg} ->
	    {error, Data, ["unable to decode data element"|Msg]};
	{ok, Res, Rest} ->
	    decode(Strategy, Rest, [Res])
    end;
decode(Strategy, Data, Acc) ->
    case wolfpacs_data_element:decode(Strategy, Data) of
	{error, _, __} ->
	    {ok, maps:from_list(Acc), Data};
	{ok, Res, Rest} ->
	    decode(Strategy, Rest, [Res|Acc])
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

encode_decode_common(Strategy) ->
    UID = <<"1.2.3.4">>,
    Items = #{{?CMD, ?UID} => UID,
	      {?CMD, ?FLD} => 16#8030,
	      {?CMD, ?RPID} => ?RQID,
	      {?CMD, ?SET} => 16#0101,
	      {?CMD, ?STU} => 16#0000},
    Encoded0 = encode(Strategy, Items),
    [ ?_assertEqual(decode(Strategy, Encoded0), {ok, Items, <<>>}) ].


encode_decode_explicit_little_test_() ->
    Strategy = {explicit, little},
    encode_decode_common(Strategy).

encode_decode_explicit_big_test_() ->
    Strategy = {explicit, big},
    encode_decode_common(Strategy).

encode_decode_implicit_little_test_() ->
    Strategy = {implicit, little},
    encode_decode_common(Strategy).

encode_decode_implicit_big_test_() ->
    Strategy = {implicit, big},
    encode_decode_common(Strategy).

encode_decode_empty_test() ->
    Strategy = {explicit, little},
    Items = #{},
    Encoded0 = encode(Strategy, Items),
    ?assertEqual(decode(Strategy, Encoded0), {error, <<>>, ["no data to decode"]}).
