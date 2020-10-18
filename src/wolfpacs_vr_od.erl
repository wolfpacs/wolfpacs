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
%% @doc Value Representation OD.
%%
%% A string of 64-bit IEEE 754:1985 floating point words.
%% OD is a VR that requires byte swapping within each 64-bit
%% word when changing between Little Endian and Big Endian
%% byte ordering (see Section 7.3).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_od).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), list(float())) -> binary().
encode(_Flow, Strategy, List) ->
    priv_encode(Strategy, List, <<>>).

-spec decode(flow(), strategy(), binary()) -> {ok, list(float()), binary()} | error.
decode(_Flow, Strategy, Data) ->
    case priv_decode(Strategy, Data, []) of
	error ->
	    error;
	Result ->
	    {ok, Result, <<>>}
    end.

%%==============================================================================
%% Private
%%==============================================================================

priv_encode(_, [], Acc) ->
    Acc;
priv_encode(Strategy = {_, little}, [Head|Tail], Acc) ->
    priv_encode(Strategy, Tail, <<Acc/binary, Head:64/float-little>>);
priv_encode(Strategy = {_, big}, [Head|Tail], Acc) ->
    priv_encode(Strategy, Tail, <<Acc/binary, Head:64/float-big>>).

priv_decode(_, <<>>, Acc) ->
    lists:reverse(Acc);
priv_decode(Strategy = {_, little}, <<Val:64/float-little, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(Strategy = {_, big}, <<Val:64/float-big, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(_, _, _) ->
    error.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(Strategy, Data) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded = encode(Flow, Strategy, Data),
    [ ?_assertEqual(decode(Flow, Strategy, Encoded), {ok, Data, <<>>}) ].

encode_decode_little_test_() ->
    Data = [1.1, 2.2, 3.3, 4.4, 5.5],
    Strategy = {explicit, little},
    encode_decode_common(Strategy, Data).

encode_decode_big_test_() ->
    Data = [1.1, 2.2, 3.3, 4.4, 5.5],
    Strategy = {explicit, big},
    encode_decode_common(Strategy, Data).
