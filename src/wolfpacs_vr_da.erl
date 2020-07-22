%%%-------------------------------------------------------------------
%% @doc Value Representation Date.
%%
%% A string of characters of the format YYYYMMDD;
%% where YYYY shall contain year, MM shall contain the month,
%% and DD shall contain the day, interpreted as a date of the
%% Gregorian calendar system.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_da).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 64).
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
