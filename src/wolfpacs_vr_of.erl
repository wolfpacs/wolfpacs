%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%
%% @doc Value Representation OF.
%%
%% A string of 32-bit IEEE 754:1985 floating point words.
%% OF is a VR that requires byte swapping within each 32-bit word
%% when changing between Little Endian and Big Endian byte ordering
%% (see Section 7.3).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_of).
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
    priv_encode(Strategy, Tail, <<Acc/binary, Head:32/float-little>>);
priv_encode(Strategy = {_, big}, [Head|Tail], Acc) ->
    priv_encode(Strategy, Tail, <<Acc/binary, Head:32/float-big>>).

priv_decode(_, <<>>, Acc) ->
    lists:reverse(Acc);
priv_decode(Strategy = {_, little}, <<Val:32/float-little, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(Strategy = {_, big}, <<Val:32/float-big, Rest/bitstring>>, Acc) ->
    priv_decode(Strategy, Rest, [Val|Acc]);
priv_decode(_, _, _) ->
    error.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(Strategy) ->
    Data = [1.0, 2.0, 3.0, 4.0, 5.0],
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded = encode(Flow, Strategy, Data),
    [ ?_assertEqual(decode(Flow, Strategy, Encoded), {ok, Data, <<>>}) ].

encode_decode_little_test_() ->
    Strategy = {explicit, little},
    encode_decode_common(Strategy).

encode_decode_big_test_() ->
    Strategy = {explicit, big},
    encode_decode_common(Strategy).
