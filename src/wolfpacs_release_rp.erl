%%%-------------------------------------------------------------------
%% @doc Release Response (RP).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_release_rp).
-export([encode/0,
	 encode/1,
	 decode/1]).

-spec encode() -> binary().
encode() ->
    encode(<<0, 0, 0, 0>>).

-spec encode(binary()) -> binary().
encode(R) ->
    <<16#6, 0, 4:32, R:32/bitstring>>.

-spec decode(binary()) -> {ok, binary(), binary()} | {error, binary()}.
decode(<<16#6, _, 4:32, R:32/bitstring, Rest/binary>>) ->
    {ok, R, Rest};
decode(Data) ->
    {error, Data, ["incorrect header"]}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    R = <<0, 0, 0, 0>>,
    Encoded0 = encode(R),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,

    [?_assertEqual(decode(Encoded0), {ok, R, <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, R, <<42>>}),
     ?_assertEqual(decode(Incorrect0), {error, Incorrect0, ["incorrect header"]}),
     ?_assertEqual(decode(Incorrect1), {error, Incorrect1, ["incorrect header"]})].
