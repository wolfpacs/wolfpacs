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
%% @doc Version Name.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_version_name).
-export([encode/2,
	 decode/2]).

-include("wolfpacs_types.hrl").

-spec encode(flow(), binary()) -> <<_:32, _:_*8>>.
encode(_Flow, VersionName) ->
    Length = byte_size(VersionName),
    TrimmedVersionName = wolfpacs_vr_utils:trim(VersionName),
    <<16#55,
      0,
      Length:16,
      TrimmedVersionName/binary>>.

-spec decode(flow(), binary()) -> {ok, binary(), binary()} | error.
decode(_Flow, <<16#55, _, Length:16, Data/binary>>) ->
    %% We are lenient, we will accept the Version Names
    %% even if they are longer than 16.
    wolfpacs_utils:split(Data, Length);
decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

%%==============================================================================
%% Private
%%==============================================================================

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    Value = <<"OFFIS_DCMTK_364">>,
    Encoded0 = encode(no_flow, Value),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = <<1,2,3,4>>,
    [ ?_assertEqual(decode(no_flow, Encoded0), {ok, Value, <<>>}),
      ?_assertEqual(decode(no_flow, Encoded1), {ok, Value, <<42>>}),
      ?_assertEqual(decode(no_flow, Incorrect0), error)
    ].
