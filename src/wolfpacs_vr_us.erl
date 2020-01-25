%%%-------------------------------------------------------------------
%% @doc Value Representation Unsigned Short (16-bit).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_us).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(strategy(), integer()) -> binary().
encode({_, little}, US) ->
    <<US:16/little>>;
encode({_, big}, US) ->
    <<US:16/big>>.

-spec decode(strategy(), binary()) -> integer().
decode({_, little}, <<US:16/little>>) ->
    US;
decode({_, big}, <<US:16/big>>) ->
    US.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_little_test() ->
    S = {explicit, little},
    ?assertEqual(decode(S, encode(S, 1024)), 1024).

encode_decode_big_test() ->
    S = {explicit, big},
    ?assertEqual(decode(S, encode(S, 1024)), 1024).
