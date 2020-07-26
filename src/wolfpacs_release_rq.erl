%%%-------------------------------------------------------------------
%% @doc Release Request (RQ).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_release_rq).
-export([encode/0,
	 encode/1,
	 decode/1]).

encode() ->
    encode(<<0, 0, 0, 0>>).

-spec encode(binary()) -> <<_:80>>.
encode(R) ->
    <<16#5, 0, 4:32, R:32/bitstring>>.

-spec decode(binary()) -> {ok, binary(), binary()} | error.
decode(<<16#5, _, 4:32, R:32/bitstring, Rest/binary>>) ->
    {ok, R, Rest};
decode(_Data) ->
    error.

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
     ?_assertEqual(decode(Incorrect0), error),
     ?_assertEqual(decode(Incorrect1), error)
    ].
