%%%-------------------------------------------------------------------
%% @doc Value Representation Long String.
%%
%% Multiple venders break the limit of 64. In order for us to route
%% correctly, we have change our limit to 128.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_lo).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 128).
-define(PAD, " ").

encode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:encode_limit(Flow, ?MODULE, X, ?LIMIT, ?PAD).

decode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, X).

%%==============================================================================
%% Private
%%==============================================================================


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
