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
%% @doc Value Representation XS.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_xs).
-export([encode/3,
	 decode/3]).

encode(Flow, {_, Endian}, {Endian, Blob}) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode XS"),
    Blob;

encode(Flow, {_, little}, Value) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode XS"),
    <<Value:16/little-unsigned>>;

encode(Flow, {_, big}, Value) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode XS"),
    <<Value:16/big-unsigned>>.

decode(Flow, {_, Endian}, Blob) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode XS"),
    {ok, {Endian, Blob}, <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Value = <<1:16/little-signed>>,
    Strategy = {explicit, little},
    Encoded0 = encode(no_flow, Strategy, {little, Value}),

    ?assertEqual(decode(no_flow, Strategy, Encoded0), {ok, {little, Value}, <<>>}).
