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
%% @doc Value Representation Date Time.
%%
%% A concatenated date-time character string in the format:
%%  YYYYMMDDHHMMSS.FFFFFF$ZZXX
%%
%% The components of this string, from left to right, are
%%  YYYY = Year,
%%  MM = Month,
%%  DD = Day,
%%  HH = Hour (range "00" - "23"),
%%  MM = Minute (range "00" - "59"),
%%  SS = Second (range "00" - "60").
%%
%% FFFFFF = Fractional Second contains a fractional part of a second
%% as small as 1 millionth of a second (range "000000" - "999999").
%%
%% $ZZXX is an optional suffix for offset from Coordinated Universal
%% Time (UTC), where $ = "+" or "-", and ZZ = Hours and
%% XX = Minutes of offset.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_dt).
-export([encode/3,
	 decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 26).
-define(PAD, " ").

encode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:encode_limit(Flow, ?MODULE, X, ?LIMIT, ?PAD).

decode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, X).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = <<"WolfPACS">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).

encode_decode_emtpy_test() ->
    Data = <<"WolfPACS">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).
