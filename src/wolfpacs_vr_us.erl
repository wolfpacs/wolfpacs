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
%% @doc Value Representation Unsigned Short (16-bit).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_us).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

encode(Flow, Strategy, Values) when is_list(Values) ->
    Parts = [ encode(Flow, Strategy, US) || US <- Values ],
    F = fun(Part, Acc) -> <<Acc/binary, Part/binary>> end,
    lists:foldl(F, <<>>, Parts);
encode(_Flow, {_, little}, US) ->
    <<US:16/little>>;
encode(_Flow, {_, big}, US) ->
    <<US:16/big>>.

decode(Flow, Strategy, Data) when byte_size(Data) > 2 ->
    Parts = wolfpacs_utils:chunk(Data, 2),
    F = fun(Part) -> decode(Flow, Strategy, Part) end,
    wolfpacs_utils:flatten_decoded(lists:map(F, Parts));
decode(_Flow, {_, little}, <<US:16/little, _Rest/binary>>) ->
    {ok, US, <<>>};
decode(_Flow, {_, big}, <<US:16/big, _Rest/binary>>) ->
    {ok, US, <<>>};
decode(Flow, _Strategy, Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode US"),
    wolfpacs_flow:expected_16(Flow, ?MODULE, Data),
    wolfpacs_flow:bad(Flow, ?MODULE, Data),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_little_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    S = {explicit, little},
    ?assertEqual(decode(Flow, S, encode(Flow, S, 1024)), {ok, 1024, <<>>}).

encode_decode_big_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    S = {explicit, big},
    ?assertEqual(decode(Flow, S, encode(Flow, S, 1024)), {ok, 1024, <<>>}).

decode_error_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    S = {explicit, little},
    [ ?_assertEqual(decode(Flow, S, <<>>), error)
    ].

encode_decode_vm_test_() ->
    S = {explicit, little},
    Values = [1, 16, 256, 1024],
    Encoded = encode(no_flow, S, Values),
    Result = decode(no_flow, S, Encoded),
    [ ?_assertEqual(Result, {ok, Values, <<>>})
    ].
