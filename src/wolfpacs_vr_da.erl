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
-export([encode/2, decode/2]).

encode(_Strategy, DA) ->
    encode(DA).

decode(_Strategy, DA) ->
    decode(DA).

%%==============================================================================
%% Private
%%==============================================================================

encode(DA) when is_list(DA)->
    list_to_binary(DA);
encode(DA) ->
    DA.

decode(<<DA:64/bitstring, Rest/binary>>) ->
    {ok, DA, Rest};
decode(OrgData) ->
    {error, OrgData, ["incorrect header"]}.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    DA0 = <<"20070102">>,
    DAL = "20070102",
    [ ?_assertEqual(decode(encode(not_important, DA0)), {ok, DA0, <<>>})
    , ?_assertEqual(decode(encode(not_important, DAL)), {ok, DA0, <<>>})
    ].
