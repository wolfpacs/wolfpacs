%%%-------------------------------------------------------------------
%% @doc Value Representation Code String.
%%
%% A string of characters with leading or trailing spaces (20H)
%% being non-significant.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_cs).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 16).
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
