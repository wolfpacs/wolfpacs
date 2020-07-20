%%%-------------------------------------------------------------------
%% @doc Value Representation Unknown.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_un).
-export([encode/3,
	 decode/3]).

encode(Flow, _Strategy, UN) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, UN, 0).

decode(Flow, _Strategy, UN) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, UN).
