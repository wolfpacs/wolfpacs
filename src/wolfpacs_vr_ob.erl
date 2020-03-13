%%%-------------------------------------------------------------------
%% @doc Value Representation OB.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ob).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(strategy(), list()) -> binary().
encode(_, List) ->
    list_to_binary(List).

-spec decode(strategy(), binary()) -> list().
decode(_, Data) ->
    {ok, binary_to_list(Data), <<>>}.

%%==============================================================================
%% Private
%%==============================================================================


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
