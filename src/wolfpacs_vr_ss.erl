%%%-------------------------------------------------------------------
%% @doc
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ss).
-export([encode/3,  decode/3]).

encode(Flow, _Strategy, UI) ->
    wolfpacs_vr_common:encode_with_limit(Flow, ?MODULE, 64, UI).

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
