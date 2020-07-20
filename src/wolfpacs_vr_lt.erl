%%%-------------------------------------------------------------------
%% @doc Value Representation Long Text.
%%
%% Leading spaces are considered to be significant.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_lt).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 10240).
-define(PAD, " ").

-spec encode(flow(), strategy(), binary()) -> binary().
encode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:encode_limit(Flow, ?MODULE, X, ?LIMIT, ?PAD).

-spec decode(flow(), strategy(), binary()) -> {ok, binary(), binary()} | error.
decode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, X).

%%==============================================================================
%% Private
%%==============================================================================


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
