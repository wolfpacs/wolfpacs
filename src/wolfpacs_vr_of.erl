%%%-------------------------------------------------------------------
%% @doc Value Representation OF.
%%
%% A string of 32-bit IEEE 754:1985 floating point words.
%% OF is a VR that requires byte swapping within each 32-bit word
%% when changing between Little Endian and Big Endian byte ordering
%% (see Section 7.3).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_of).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), list(float())) -> binary().
encode(_Flow, Strategy, List) ->
    priv_encode(Strategy, List, <<>>).

-spec decode(flow(), strategy(), binary()) -> {ok, list(float()), binary()} | error.
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
    priv_encode(Strategy, Tail, <<Acc/binary, Head:32/float-little>>);
priv_encode(Strategy = {_, big}, [Head|Tail], Acc) ->
    priv_encode(Strategy, Tail, <<Acc/binary, Head:32/float-big>>).

priv_decode(_, <<>>, Acc) ->
    lists:reverse(Acc);
priv_decode(Strategy = {_, little}, <<Val:32/float-little, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(Strategy = {_, big}, <<Val:32/float-big, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(_, _, _) ->
    error.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(Strategy) ->
    Data = [1.0, 2.0, 3.0, 4.0, 5.0],
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded = encode(Flow, Strategy, Data),
    [ ?_assertEqual(decode(Flow, Strategy, Encoded), {ok, Data, <<>>}) ].

encode_decode_little_test_() ->
    Strategy = {explicit, little},
    encode_decode_common(Strategy).

encode_decode_big_test_() ->
    Strategy = {explicit, big},
    encode_decode_common(Strategy).
