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
