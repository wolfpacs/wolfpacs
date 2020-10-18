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
