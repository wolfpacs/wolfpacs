%%%-------------------------------------------------------------------
%% @doc Data Elements.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_elements).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(pid(), strategy(), map() | list()) -> binary().
encode(Flow, Strategy, Info) when is_map(Info) ->
    encode(Flow, Strategy, maps:to_list(Info));
encode(Flow, Strategy, Elements) ->
    encode(Flow, Strategy, lists:sort(Elements), <<>>).

-spec decode(pid(), strategy(), binary()) -> {ok, map(), binary()} | {error, binary(), list(string())}.
decode(Flow, Strategy, Data) ->
    decode(Flow, Strategy, Data, []).

%%==============================================================================
%% Private Encoders
%%==============================================================================

encode(_Flow, _, [], Acc) ->
    Acc;
encode(Flow, Strategy, [{{G, E}, Data}|Rest], Acc) ->
    VR = wolfpacs_group_elements:vr(G, E),
    Encoded = wolfpacs_data_element:encode(Flow, Strategy, G, E, VR, Data),
    encode(Flow, Strategy, Rest, <<Acc/binary, Encoded/binary>>);
encode(Flow, Strategy, [{{G, E, VR}, Data}|Rest], Acc) ->
    Encoded = wolfpacs_data_element:encode(Flow, Strategy, G, E, VR, Data),
    encode(Flow, Strategy, Rest, <<Acc/binary, Encoded/binary>>).

-spec decode(pid(), strategy(), binary(), list()) -> {ok, map(), binary()} | {error, binary(), list(string())}.
decode(Flow, _, <<>>, []) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "no data to decode"),
    error;

decode(_Flow, _, <<>>, Acc) ->
    {ok, maps:from_list(Acc), <<>>};
decode(Flow, _, Rest, [{{16#fffe, 16#e00d}, _}|Acc]) ->
    wolfpacs_flow:good(Flow, ?MODULE, "Found item delimitation"),
    {ok, maps:from_list(Acc), Rest};
decode(Flow, _, Rest, [{{16#fffe, 16#e0dd}, _}|Acc]) ->
    wolfpacs_flow:good(Flow, ?MODULE, "Found sequence delimitation"),
    {ok, maps:from_list(Acc), Rest};
decode(Flow, Strategy, Data, Acc) ->
    case wolfpacs_data_element:decode(Flow, Strategy, Data) of
	{ok, Res, Rest} ->
	    decode(Flow, Strategy, Rest, [Res|Acc]);
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "failed to decode data element"),
	    error
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
    {ok, Flow} = wolfpacs_flow:start_link(),
    UID = <<"1.2.3.4">>,
    Items = #{{?CMD, ?UID} => UID,
	      {?CMD, ?FLD} => 16#8030,
	      {?CMD, ?RPID} => ?RQID,
	      {?CMD, ?SET} => 16#0101,
	      {?CMD, ?STU} => 16#0000},

    Encoded0 = encode(Flow, Strategy, Items),

    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect3 = <<1, 2, 3, 4, 5>>,
    Incorrect4 = <<>>,

    [ ?_assertEqual(decode(Flow, Strategy, Encoded0), {ok, Items, <<>>})
    , ?_assertEqual(decode(Flow, Strategy, Incorrect1), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect2), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect3), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect4), error)
    ].


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
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    Items = #{},
    Encoded0 = encode(Flow, Strategy, Items),
    ?assertEqual(decode(Flow, Strategy, Encoded0), error).
