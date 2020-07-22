%%%-------------------------------------------------------------------
%% @doc Decimal String Value Representation
%%
%% A string of characters representing either a fixed point number or
%% a floating point number. A fixed point number shall contain only
%% the characters 0-9 with an optional leading "+" or "-" and
%% an optional "." to mark the decimal point.
%% A floating point number shall be conveyed as defined in ANSI X3.9,
%% with an "E" or "e" to indicate the start of the exponent.
%% Decimal Strings may be padded with leading or trailing spaces.
%% Embedded spaces are not allowed.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ds).
-export([encode/3,
	 decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 64).
-define(PAD, " ").

encode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:encode_limit(Flow, ?MODULE, X, ?LIMIT, ?PAD).

decode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, X).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = <<"-1.197656e02">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).

encode_decode_vm_test() ->
    Data = <<"-1.197656e02\-3.997656e02\-2.800000e02">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).
