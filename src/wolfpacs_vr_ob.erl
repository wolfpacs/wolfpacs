%%%-------------------------------------------------------------------
%% @doc Value Representation OB.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ob).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), list()) -> binary().
encode(_Flow, _, List) ->
    list_to_binary(List).

-spec decode(flow(), strategy(), binary()) -> list().
decode(_Flow, _, Data) ->
    {ok, binary_to_list(Data), <<>>}.

%%==============================================================================
%% Private
%%==============================================================================


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
