%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
%%
%%%-------------------------------------------------------------------
%% @doc Value Representation Signed Short (SS).
%%
%% Signed binary integer 16 bits long in 2's complement form.
%% Represents an integer n in the range: n in [-128, 127]
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ss).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(BYTE_SIZE, 2).
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
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode SS"),
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
    Values = [1, 16, 256, 1024],
    Encoded = encode(no_flow, S, Values),
    Result = decode(no_flow, S, Encoded),
    [ ?_assertEqual(Result, {ok, Values, <<>>})
    ].
