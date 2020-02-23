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
-export([encode/4, decode/2]).
-import(wolfpacs_vr_utils, [trim_binary/1]).

encode(_Strategy, Year, Month, Day) ->
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
decode(Data) ->
    trim_binary(Data).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [?_assertEqual(encode(2007, 1, 1), <<"20070101">>)
    ].
