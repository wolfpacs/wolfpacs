%%%-------------------------------------------------------------------
%% @doc Value Representation Time.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_tm).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 16).
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
