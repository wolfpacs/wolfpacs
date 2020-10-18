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
%% @doc Value Representation Signed Long (SL).
%%
%% Signed binary integer ?BIT_SIZE bits long in 2's complement form.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_sl).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(BYTE_SIZE, 4).
-define(BIT_SIZE, (?BYTE_SIZE * 8)).

encode(Flow, Strategy, Values) when is_list(Values) ->
    Parts = [ encode(Flow, Strategy, Value) || Value <- Values ],
    F = fun(Part, Acc) -> <<Acc/binary, Part/binary>> end,
    lists:foldl(F, <<>>, Parts);
encode(Flow, {_, little}, X) ->
    wolfpacs_flow:generated(Flow, ?MODULE, ?BYTE_SIZE),
    <<X:?BIT_SIZE/little-signed>>;
encode(Flow, {_, big}, X) ->
    wolfpacs_flow:generated(Flow, ?MODULE, ?BYTE_SIZE),
    <<X:?BIT_SIZE/big-signed>>.

decode(Flow, Strategy, Data) when byte_size(Data) > ?BYTE_SIZE ->
    Parts = wolfpacs_utils:chunk(Data, ?BYTE_SIZE),
    F = fun(Part) -> decode(Flow, Strategy, Part) end,
    wolfpacs_utils:flatten_decoded(lists:map(F, Parts));
decode(Flow, {_, little}, <<X:?BIT_SIZE/little-signed>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, ?BYTE_SIZE),
    {ok, X, <<>>};
decode(Flow, {_, big}, <<X:?BIT_SIZE/big-signed>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, ?BYTE_SIZE),
    {ok, X, <<>>};
decode(Flow, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode SL"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode(Flow, Strategy, Value) ->
    Encoded = encode(Flow, Strategy, Value),
    Decoded = decode(Flow, Strategy, Encoded),
    ?_assertEqual(Decoded, {ok, Value, <<>>}).

encode_decode_all_strategies(Flow, Value) ->
    [ encode_decode(Flow, {explicit, little}, Value)
    , encode_decode(Flow, {explicit, big}, Value)
    , encode_decode(Flow, {implicit, little}, Value)
    , encode_decode(Flow, {implicit, big}, Value)
    ].

encode_decode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    lists:flatten(
      [ encode_decode_all_strategies(Flow, 0)
      , encode_decode_all_strategies(Flow, -128)
      , encode_decode_all_strategies(Flow, 127)
      ]
     ).

encode_decode_vm_test_() ->
    S = {explicit, little},
    Values = [-1, 1, 32, 256, 1024, -1232131],
    Encoded = encode(no_flow, S, Values),
    Result = decode(no_flow, S, Encoded),
    [ ?_assertEqual(Result, {ok, Values, <<>>})
    ].
