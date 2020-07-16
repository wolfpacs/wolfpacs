%%%-------------------------------------------------------------------
%% @doc Value Representation OW.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ow).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), list()) -> binary().
encode(_Flow, Strategy, List) ->
    priv_encode(Strategy, List, <<>>).

-spec decode(flow(), strategy(), binary()) -> {ok, list(), binary()} | error.
decode(_Flow, Strategy, Data) ->
    case priv_decode(Strategy, Data, []) of
	error ->
	    error;
	Result ->
	    {ok, Result, <<>>}
    end.

%%==============================================================================
%% Private
%%==============================================================================

priv_encode(_, [], Acc) ->
    Acc;
priv_encode(Strategy = {_, little}, [Head|Tail], Acc) ->
    priv_encode(Strategy, Tail, <<Acc/binary, Head:16/little>>);
priv_encode(Strategy = {_, big}, [Head|Tail], Acc) ->
    priv_encode(Strategy, Tail, <<Acc/binary, Head:16/big>>).

priv_decode(_, <<>>, Acc) ->
    lists:reverse(Acc);
priv_decode(Strategy = {_, little}, <<Val:16/little, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(Strategy = {_, big}, <<Val:16/big, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(_, _, _) ->
    error.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(Strategy, Data) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded = encode(Flow, Strategy, Data),
    [ ?_assertEqual(decode(Flow, Strategy, Encoded), {ok, Data, <<>>}) ].

encode_decode_little_test_() ->
    Data = [1, 2, 3, 4, 5],
    Strategy = {explicit, little},
    encode_decode_common(Strategy, Data).

encode_decode_big_test_() ->
    Data = [1, 2, 3, 4, 5],
    Strategy = {explicit, big},
    encode_decode_common(Strategy, Data).
