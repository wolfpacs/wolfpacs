%%%-------------------------------------------------------------------
%% @doc Value Representation OB.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_of).
-export([encode_little/1,
	 decode_little/1,
	 encode_big/1,
	 decode_big/1]).

-spec encode_little(list()) -> binary().
encode_little(List) ->
    encode_little(List, <<>>).

-spec decode_little(binary()) -> list().
decode_little(Data) ->
    decode_little(Data, []).

-spec encode_big(list()) -> binary().
encode_big(List) ->
    encode_big(List, <<>>).

-spec decode_big(binary()) -> list().
decode_big(Data) ->
    decode_big(Data, []).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode_little(list(), binary()) -> binary().
encode_little([], Acc) ->
    Acc;
encode_little([H|T], Acc) ->
    encode_little(T, <<Acc/binary, H:32/little>>).

decode_little(<<>>, Acc) ->
    lists:reverse(Acc);
decode_little(<<Val:32/little, Rest/bitstring>>, Acc) ->
    decode_little(Rest, [Val|Acc]).

-spec encode_big(list(), binary()) -> binary().
encode_big([], Acc) ->
    Acc;
encode_big([H|T], Acc) ->
    encode_big(T, <<Acc/binary, H:32/big>>).

decode_big(<<>>, Acc) ->
    lists:reverse(Acc);
decode_big(<<Val:32/big, Rest/bitstring>>, Acc) ->
    decode_big(Rest, [Val|Acc]).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_little_test_() ->
    Data = [1, 2, 3, 4, 5],
    [ ?_assertEqual(decode_little(encode_little(Data)), Data) ].

encode_decode_big_test_() ->
    Data = [1, 2, 3, 4, 5],
    [ ?_assertEqual(decode_big(encode_big(Data)), Data) ].
