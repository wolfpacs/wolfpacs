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
%% @doc Release Response (RP).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_release_rp).
-export([encode/0,
	 encode/1,
	 decode/1]).

encode() ->
    encode(<<0, 0, 0, 0>>).

-spec encode(binary()) -> <<_:80>>.
encode(R) ->
    <<16#6, 0, 4:32, R:32/bitstring>>.

-spec decode(binary()) -> {ok, binary(), binary()} | error.
decode(<<16#6, _, 4:32, R:32/bitstring, Rest/binary>>) ->
    {ok, R, Rest};
decode(_Data) ->
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    R = <<0, 0, 0, 0>>,
    Encoded0 = encode(R),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,

    [?_assertEqual(decode(Encoded0), {ok, R, <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, R, <<42>>}),
     ?_assertEqual(decode(Incorrect0), error),
     ?_assertEqual(decode(Incorrect1), error)].
