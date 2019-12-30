%%%-------------------------------------------------------------------
%% @doc Release Response (RP)
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_release_rp).
-export([encode/0,
	 encode/1,
	 decode/1]).

encode() ->
    encode(0).

encode(R) ->
    <<16#6, 0, 4:32, R:32>>.

decode(<<16#6, 0, 4:32, R:32>>) ->
    {ok, R};
decode(Data) ->
    {error, Data}.

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    [?_assertEqual(decode(encode()), {ok, 0})].
