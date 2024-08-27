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
%% @doc Integer String Value Representation
%%
%% A string of characters representing an Integer in base-10 (decimal),
%% shall contain only the characters 0 - 9, with an optional
%% leading "+" or "-". It may be padded with leading and/or trailing spaces.
%% Embedded spaces are not allowed.
%%
%% The integer, n, represented shall be in the range: [-231, 230]
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_is).
-export([encode/3,
	 decode/3]).

encode(Flow, Strategy, IS) when is_integer(IS) ->
    encode(Flow, Strategy, integer_to_binary(IS));
encode(Flow, _Strategy, IS) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, IS, " ").

decode(Flow, _Strategy, Data) ->
    case wolfpacs_vr_common:decode(Flow, ?MODULE, Data) of
	{ok, ValueString, <<>>} ->
	    try
		Value = binary_to_integer(ValueString),
		{ok, Value, <<>>}
	    catch
		error:badarg -> error
	    end;
	_ ->
	    error
    end.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Strategy = {explicit, little},

    Encoded0 = encode(no_flow, Strategy, -23),
    Encoded1 = encode(no_flow, Strategy, <<"-23">>),
    Encoded2 = encode(no_flow, Strategy, <<"-2foo3">>),

    [ ?_assertEqual(decode(no_flow, Strategy, Encoded0), {ok, -23, <<>>})
    , ?_assertEqual(decode(no_flow, Strategy, Encoded1), {ok, -23, <<>>})
    , ?_assertEqual(decode(no_flow, Strategy, Encoded2), error)
    ].

encode_decode_error_test() ->
    Data = <<"Banana">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    Result = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(error, Result).
