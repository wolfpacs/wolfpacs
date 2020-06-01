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
    priv_encode(Flow, DA).

decode(Flow, _Strategy, DA) ->
    priv_decode(Flow, DA).

%%==============================================================================
%% Private
%%==============================================================================

priv_encode(_Flow, DA) when is_list(DA)->
    list_to_binary(DA);
priv_encode(_Flow, DA) ->
    DA.

priv_decode(_Flow, <<DA:64/bitstring, Rest/binary>>) ->
    {ok, DA, Rest};
priv_decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    DA0 = <<"20070102">>,
    DAL = "20070102",
    S = {implicit, little},
    [ ?_assertEqual(decode(Flow, S, encode(Flow, S, DA0)), {ok, DA0, <<>>})
    , ?_assertEqual(decode(Flow, S, encode(Flow, S, DAL)), {ok, DA0, <<>>})
    ].
