%%%-------------------------------------------------------------------
%% @doc Value Representation Age String.
%%
%% A string of characters with one of the following formats --
%% nnnD,
%% nnnW,
%% nnnM,
%% nnnY;
%%
%% where nnn shall contain the number of days for D,
%% weeks for W, months for M, or years for Y.
%% Example: "018M" would represent an age of 18 months.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_as).
-export([encode/3, decode/3]).

encode(Flow, _Strategy, AS) ->
    wolfpacs_vr_common:encode_with_limit(Flow, ?MODULE, 4, AS).

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).
