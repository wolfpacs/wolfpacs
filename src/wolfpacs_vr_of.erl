%%%-------------------------------------------------------------------
%% @doc Value Representation OB.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_of).
-export([encode_little/1,
	 decode_little/1]).

-spec encode_little(list()) -> binary().
encode_little(List) ->
    encode_little(List, <<>>).

-spec decode_little(binary()) -> list().
decode_little(Data) ->
    decode_little(Data, []).

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

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Data = [1, 2, 3, 4, 5],
    [ ?_assertEqual(decode_little(encode_little(Data)), Data) ].
