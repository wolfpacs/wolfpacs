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
