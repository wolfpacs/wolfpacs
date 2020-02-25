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

encode(_Strategy, {Year, Month, Day}) ->
    encode(Year, Month, Day).

decode(_Strategy, DA) ->
    decode(DA).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(pos_integer(), pos_integer(), pos_integer()) -> binary().
encode(Year, Month, Day) ->
    DateString = io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day]),
    list_to_binary(DateString).

-spec decode(binary()) -> binary().
decode(<<Year:32/bitstring, Month:16/bitstring, Day:16/bitstring>>) ->
    {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [ ?_assertEqual(encode(not_important, {2007, 1, 2}), <<"20070102">>)
    , ?_assertEqual(decode(not_important, <<"20070102">>), {2007, 1, 2})
    ].
