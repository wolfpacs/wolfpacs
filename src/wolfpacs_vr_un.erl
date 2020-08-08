%%%-------------------------------------------------------------------
%% @doc Value Representation Unknown.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_un).
-export([encode/3,
	 decode/3]).

-include("wolfpacs_types.hrl").

encode(Flow, _Strategy, UN) when is_binary(UN) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, UN, 0);
encode(Flow, _Strategy, UN) when is_list(UN) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, list_to_binary(UN), 0).

decode(Flow, _Strategy, UN) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, UN).
