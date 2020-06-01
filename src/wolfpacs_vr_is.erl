%%%-------------------------------------------------------------------
%% @doc
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_is).
-export([encode/3,
	 decode/3]).

encode(Flow, _Strategy, AE) ->
    wolfpacs_vr_common:encode_with_limit(Flow, ?MODULE, 64, AE).

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).
