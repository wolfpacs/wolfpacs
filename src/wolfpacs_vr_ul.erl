%%%-------------------------------------------------------------------
%% @doc Value Representation Unsigned Long (32-bit).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ul).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), integer()) -> binary().
encode(Flow, {_, little}, UL) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<UL:32/little>>;
encode(Flow, {_, big}, UL) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<UL:32/big>>.

-spec decode(flow(), strategy(), binary()) -> integer().
decode(Flow, {_, little}, <<UL:32/little>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, 4),
    {ok, UL, <<>>};
decode(Flow, {_, big}, <<UL:32/big>>) ->
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
