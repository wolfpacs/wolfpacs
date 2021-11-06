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
%% @doc Implementation Class.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_implementation_class).
-export([encode/1,
	 decode/1]).
-import(wolfpacs_utils, [split/2]).

-spec encode(binary()) -> binary() | {error, too_short} | {error, too_long}.
encode(ImplementationClass) ->
    Length = byte_size(ImplementationClass),
    <<16#52,
      0,
      Length:16,
      ImplementationClass/binary>>.

-spec decode(binary()) -> {ok, binary(), binary()} | {error, binary()}.
decode(<<16#52, _, Length:16, Data/binary>>) ->
    split(Data, Length);
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    Value = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    EncodedC = <<82,0,0,27,
		 49,46,50,46,50,55,54,46,48,46,55,50,51,
		 48,48,49,48,46,51,46,48,46,51,46,54,46,52>>,
    Encoded0 = encode(Value),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = <<1,2,3,4>>,
    [ ?_assertEqual(decode(EncodedC), {ok, Value, <<>>}),
      ?_assertEqual(decode(Encoded0), {ok, Value, <<>>}),
      ?_assertEqual(decode(Encoded1), {ok, Value, <<42>>}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0}) ].
