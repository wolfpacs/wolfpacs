%%%-------------------------------------------------------------------
%% @doc Value Representation OW.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ow).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(strategy(), list()) -> binary().
encode(Strategy, List) ->
    encode(Strategy, List, <<>>).

-spec decode(strategy(), binary()) -> list().
decode(Strategy, Data) ->
    decode(Strategy, Data, []).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(strategy(), list(), binary()) -> binary().
encode(_, [], Acc) ->
    Acc;
encode(Strategy = {_, little}, [Head|Tail], Acc) ->
    encode(Strategy, Tail, <<Acc/binary, Head:16/little>>);
encode(Strategy = {_, big}, [Head|Tail], Acc) ->
    encode(Strategy, Tail, <<Acc/binary, Head:16/big>>).

decode(_, <<>>, Acc) ->
    lists:reverse(Acc);
decode(Strategy = {_, little}, <<Val:16/little, Rest/bitstring>>, Acc) ->
    decode(Strategy, Rest, [Val|Acc]);
decode(Strategy = {_, big}, <<Val:16/big, Rest/bitstring>>, Acc) ->
    decode(Strategy, Rest, [Val|Acc]).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(Strategy, Data) ->
    Encoded = encode(Strategy, Data),
    [ ?_assertEqual(decode(Strategy, Encoded), Data) ].

encode_decode_little_test_() ->
    Data = [1, 2, 3, 4, 5],
    Strategy = {explicit, little},
    encode_decode_common(Strategy, Data).

encode_decode_big_test_() ->
    Data = [1, 2, 3, 4, 5],
    Strategy = {explicit, big},
    encode_decode_common(Strategy, Data).
