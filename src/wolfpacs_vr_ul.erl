%%%-------------------------------------------------------------------
%% @doc Value Representation Unsigned Long (32-bit).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ul).
-export([encode_little/1,
	 decode_little/1,
	 encode_big/1,
	 decode_big/1]).

-spec encode_little(integer()) -> binary().
encode_little(US) ->
    <<US:32/little>>.

-spec decode_little(binary()) -> integer().
decode_little(<<US:32/little>>) ->
    US.

-spec encode_big(integer()) -> binary().
encode_big(US) ->
    <<US:32/big>>.

-spec decode_big(binary()) -> integer().
decode_big(<<US:32/big>>) ->
    US.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_little_test() ->
    ?assertEqual(decode_little(encode_little(1024)), 1024).

encode_decode_big_test() ->
    ?assertEqual(decode_big(encode_big(1024)), 1024).
