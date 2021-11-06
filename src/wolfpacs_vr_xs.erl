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
