%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
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
