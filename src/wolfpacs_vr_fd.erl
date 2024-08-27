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
%% @doc Value Representation Float Double (FD).
%%
%% Double precision binary floating point number represented in
%% IEEE 754:1985 64-bit Floating Point Number Format.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_fd).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

-define(BYTE_SIZE, 8).
-define(BIT_SIZE, (?BYTE_SIZE * 8)).

encode(Flow, Strategy, Values) when is_list(Values) ->
    Parts = [ encode(Flow, Strategy, Value) || Value <- Values ],
    F = fun(Part, Acc) -> <<Acc/binary, Part/binary>> end,
    lists:foldl(F, <<>>, Parts);

encode(Flow, {_, little}, X) ->
    wolfpacs_flow:generated(Flow, ?MODULE, ?BYTE_SIZE),
    <<X:?BIT_SIZE/float-little>>;

encode(Flow, {_, big}, X) ->
    wolfpacs_flow:generated(Flow, ?MODULE, ?BYTE_SIZE),
    <<X:?BIT_SIZE/float-big>>.

decode(Flow, Strategy, Data) when byte_size(Data) > ?BYTE_SIZE ->
    Parts = wolfpacs_utils:chunk(Data, ?BYTE_SIZE),
    F = fun(Part) -> decode(Flow, Strategy, Part) end,
    wolfpacs_utils:flatten_decoded(lists:map(F, Parts));

decode(Flow, {_, little}, <<X:?BIT_SIZE/float-little>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, ?BYTE_SIZE),
    {ok, X, <<>>};

decode(Flow, {_, big}, <<X:?BIT_SIZE/float-big>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, ?BYTE_SIZE),
    {ok, X, <<>>};

decode(Flow, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode"),
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
      [ encode_decode_all_strategies(Flow, 1.0)
      , encode_decode_all_strategies(Flow, -1.0)
      , encode_decode_all_strategies(Flow, 12.12)
      ]
     ).

encode_decode_vm_test_() ->
    S = {explicit, little},
    Values = [1.0, -1.0, 1024.0],
    Encoded = encode(no_flow, S, Values),
    Result = decode(no_flow, S, Encoded),
    [ ?_assertEqual(Result, {ok, Values, <<>>})
    ].
