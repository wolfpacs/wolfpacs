%%%-------------------------------------------------------------------
%% @doc Value Representation Patient name.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_pn).
-export([encode/3,
	 decode/3]).

encode(Flow, _Strategy, PN) ->
    wolfpacs_vr_common:encode_binary_with_limit(Flow, ?MODULE, 64, PN).

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode_binary(Flow, ?MODULE, Data).
