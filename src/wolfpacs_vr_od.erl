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
