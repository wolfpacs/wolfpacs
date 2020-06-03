%%%-------------------------------------------------------------------
%% @doc
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ds).
-export([encode/3,
	 decode/3]).

encode(Flow, _Strategy, AE) ->
    wolfpacs_vr_common:encode_with_limit(Flow, ?MODULE, 64, AE).

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = <<"example">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).
