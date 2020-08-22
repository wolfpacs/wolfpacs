%%%-------------------------------------------------------------------
%% @doc Value Representation Unsigned Long (32-bit).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ul).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

encode(Flow, Strategy, Values) when is_list(Values) ->
    Parts = [ encode(Flow, Strategy, Value) || Value <- Values ],
    F = fun(Part, Acc) -> <<Acc/binary, Part/binary>> end,
    lists:foldl(F, <<>>, Parts);
encode(Flow, {_, little}, UL) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<UL:32/little-unsigned>>;
encode(Flow, {_, big}, UL) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<UL:32/big-unsigned>>.

decode(Flow, Strategy, Data) when byte_size(Data) > 4 ->
    Parts = wolfpacs_utils:chunk(Data, 4),
    F = fun(Part) -> decode(Flow, Strategy, Part) end,
    wolfpacs_utils:flatten_decoded(lists:map(F, Parts));
decode(Flow, {_, little}, <<UL:32/little-unsigned>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, 4),
    {ok, UL, <<>>};
decode(Flow, {_, big}, <<UL:32/big-unsigned>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, 4),
    {ok, UL, <<>>};
decode(Flow, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode UL"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_little_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    S = {explicit, little},
    ?assertEqual(decode(Flow, S, encode(Flow, S, 1024)), {ok, 1024, <<>>}).

encode_decode_big_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    S = {explicit, big},
    ?assertEqual(decode(Flow, S, encode(Flow, S, 1024)), {ok, 1024, <<>>}).

bad_decode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    S = {explicit, big},
    [ ?_assertEqual(decode(Flow, S, <<>>), error)
    , ?_assertEqual(decode(Flow, S, <<1>>), error)
    , ?_assertEqual(decode(Flow, S, <<1, 2, 3, 4, 5>>), error)
    ].

encode_decode_vm_test_() ->
    S = {explicit, little},
    Values = [1, 16, 220, 220, 256, 1024],
    Encoded = encode(no_flow, S, Values),
    Result = decode(no_flow, S, Encoded),
    [ ?_assertEqual(Result, {ok, Values, <<>>})
    ].
