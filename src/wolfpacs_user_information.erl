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
%% @doc User Information.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_user_information).
-export([encode/4,
	 decode/2]).

-include("wolfpacs_types.hrl").

-spec encode(flow(), non_neg_integer(), binary(), binary()) -> <<_:32, _:_*8>>.
encode(Flow, MaxPDUSize, Class, VersionName) ->
    A = wolfpacs_max_length:encode(MaxPDUSize),
    B = wolfpacs_implementation_class:encode(Class),
    C = wolfpacs_version_name:encode(Flow, VersionName),
    encode(<<A/binary, B/binary, C/binary>>).

-spec decode(flow(), binary()) -> {ok, non_neg_integer(), binary(), binary(), binary()} | error.
decode(Flow, <<16#50, _, _Length:16, UserInformation/binary>>) ->
    MaybeMaxLength = wolfpacs_max_length:decode(UserInformation),
    decode_with_max_length(Flow, MaybeMaxLength);
decode(Flow, <<_H, _/binary>>) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error;
decode(Flow, <<>>) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "no data"),
    error.

%%==============================================================================
%% Private
%%==============================================================================

encode(UserInformation) ->
    Length = byte_size(UserInformation),
    <<16#50,
      0,
      Length:16,
      UserInformation/binary>>.

decode_with_max_length(Flow, {ok, MaxSize, Rest}) ->
    MaybeImplementationClass = wolfpacs_implementation_class:decode(Rest),
    decode_with_implementation_class(Flow, MaxSize, MaybeImplementationClass);
decode_with_max_length(Flow, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "error with_max length"),
    error.

decode_with_implementation_class(Flow, MaxSize, {ok, ImplementationClass, Rest}) ->
    MaybeVersionName = wolfpacs_version_name:decode(Flow, Rest),
    decode_with_version_name(Flow, MaxSize, ImplementationClass, MaybeVersionName);
decode_with_implementation_class(Flow, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "error with implimentation class"),
    error.

decode_with_version_name(Flow, MaxSize, ImplementationClass, {ok, VersionName, Rest}) ->
    wolfpacs_flow:good(Flow, ?MODULE, "found all parts"),
    {ok, MaxSize, ImplementationClass, VersionName, Rest};
decode_with_version_name(Flow, _, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "error with version name"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_echoscu_test() ->
    MaxSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,
    Encoded = encode(no_flow, MaxSize, Class, VersionName),
    Correct = <<80,0,0,58,

		81,0,0,4,
		0,0,64,0,

		82,0,0,27,
		49,46,50,46,50,55,54,46,48,46,55,50,51,48,48,49,48,46,51,46,48,46,51,46,54,46,52,

		85,0,0,15,
		79,70,70,73,83,95,68,67,77,84,75,95,51,54,52>>,
    ?assertEqual(Encoded, Correct).

encode_decode_test_() ->
    MaxSize = 16384,
    Class = <<"1">>,
    VersionName = <<"A">>,
    Encoded0 = encode(no_flow, MaxSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1,2,3,4>>,
    {ok, Incorrect2} = wolfpacs_utils:clear_byte(Encoded0,  7),
    {ok, Incorrect3} = wolfpacs_utils:clear_byte(Encoded0, 12),
    {ok, Incorrect4} = wolfpacs_utils:clear_byte(Encoded0, 17),

    [ ?_assertEqual(decode(no_flow, Encoded0), {ok, MaxSize, Class, VersionName, <<>>}),
      ?_assertEqual(decode(no_flow, Encoded1), {ok, MaxSize, Class, VersionName, <<42>>}),
      ?_assertEqual(decode(no_flow, Incorrect0), error),
      ?_assertEqual(decode(no_flow, Incorrect1), error),
      ?_assertEqual(decode(no_flow, Incorrect2), error),
      ?_assertEqual(decode(no_flow, Incorrect3), error),
      ?_assertEqual(decode(no_flow, Incorrect4), error)
    ].
