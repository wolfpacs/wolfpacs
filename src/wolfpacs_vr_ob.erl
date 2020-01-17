%%%-------------------------------------------------------------------
%% @doc Value Representation OB.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ob).
-export([encode/1,
	 decode/1]).

-spec encode(list()) -> binary().
encode(List) ->
    list_to_binary(List).

-spec decode(binary()) -> list().
decode(Data) ->
    binary_to_list(Data).

%%==============================================================================
%% Private
%%==============================================================================


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Data = [1, 2, 3, 4, 5],
    [ ?_assertEqual(decode(encode(Data)), Data) ].
