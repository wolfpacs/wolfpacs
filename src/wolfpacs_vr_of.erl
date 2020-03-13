%%%-------------------------------------------------------------------
%% @doc Value Representation OW.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_of).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(strategy(), list()) -> binary().
encode(Strategy, List) ->
    encode(Strategy, List, <<>>).

-spec decode(strategy(), binary()) -> list().
decode(Strategy, Data) ->
    case decode(Strategy, Data, []) of
	error ->
	    {error, Data, ["unable to decode"]};
	Result ->
	    {ok, Result, <<>>}
    end.

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(strategy(), list(), binary()) -> binary().
encode(_, [], Acc) ->
    Acc;
encode(Strategy = {_, little}, [Head|Tail], Acc) ->
    encode(Strategy, Tail, <<Acc/binary, Head:32/little>>);
encode(Strategy = {_, big}, [Head|Tail], Acc) ->
    encode(Strategy, Tail, <<Acc/binary, Head:32/big>>).

decode(_, <<>>, Acc) ->
    lists:reverse(Acc);
decode(Strategy = {_, little}, <<Val:32/little, Rest/bitstring>>, Acc) ->
    decode(Strategy, Rest, [Val|Acc]);
decode(Strategy = {_, big}, <<Val:32/big, Rest/bitstring>>, Acc) ->
    decode(Strategy, Rest, [Val|Acc]);
decode(_, _, _) ->
    error.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(Strategy, Data) ->
    Encoded = encode(Strategy, Data),
    [ ?_assertEqual(decode(Strategy, Encoded), {ok, Data, <<>>}) ].

encode_decode_little_test_() ->
    Data = [1, 2, 3, 4, 5],
    Strategy = {explicit, little},
    encode_decode_common(Strategy, Data).

encode_decode_big_test_() ->
    Data = [1, 2, 3, 4, 5],
    Strategy = {explicit, big},
    encode_decode_common(Strategy, Data).
