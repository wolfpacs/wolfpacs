%%%-------------------------------------------------------------------
%% @doc Value Representation Code String.
%%
%% A string of characters with leading or trailing spaces (20H)
%% being non-significant.
%%
%% Multiple venders break the limit of 16. In order for us to route
%% correctly, we have change our limit to 32.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_cs).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 32). %% OUT OF SPEC
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
