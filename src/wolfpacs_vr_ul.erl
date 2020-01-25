%%%-------------------------------------------------------------------
%% @doc Value Representation Unsigned Long (32-bit).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ul).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(strategy(), integer()) -> binary().
encode({_, little}, UL) ->
    <<UL:32/little>>;
encode({_, big}, UL) ->
    <<UL:32/big>>.

-spec decode(strategy(), binary()) -> integer().
decode({_, little}, <<UL:32/little>>) ->
    UL;
decode({_, big}, <<UL:32/big>>) ->
    UL.

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
