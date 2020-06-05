%%%-------------------------------------------------------------------
%% @doc Value Representation Unsigned Short (16-bit).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_us).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), integer()) -> binary().
encode(_Flow, {_, little}, US) ->
    <<US:16/little>>;
encode(_Flow, {_, big}, US) ->
    <<US:16/big>>.

-spec decode(flow(), strategy(), binary()) -> integer().
decode(_Flow, {_, little}, <<US:16/little, _Rest/binary>>) ->
    {ok, US, <<>>};
decode(_Flow, {_, big}, <<US:16/big, _Rest/binary>>) ->
    {ok, US, <<>>};
decode(Flow, _Strategy, Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode US"),
    wolfpacs_flow:expected_16(Flow, ?MODULE, Data),
    wolfpacs_flow:bad(Flow, ?MODULE, Data),
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

decode_error_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    S = {explicit, little},
    [ ?_assertEqual(decode(Flow, S, <<>>), error)
    ].
