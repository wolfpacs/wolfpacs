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

encode(Flow, _Strategy, DA) ->
    wolfpacs_vr_common:encode_with_limit(Flow, ?MODULE, 64, DA).

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    DA0 = "20070102",
    DAL = "20070102",
    S = {implicit, little},
    [ ?_assertEqual(decode(Flow, S, encode(Flow, S, DA0)), {ok, DA0, <<>>})
    , ?_assertEqual(decode(Flow, S, encode(Flow, S, DAL)), {ok, DA0, <<>>})
    ].
