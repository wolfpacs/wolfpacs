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
%% @doc C Echo SCP.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_c_echo_scp).
-export([encode/4]).

encode(Flow, Strategy, UID, RQID) ->
    Info = #{{16#0000, 16#0002, "UI"} => UID,
	     {16#0000, 16#0100, "US"} => 16#8030,
	     {16#0000, 16#0120, "US"} => RQID,
	     {16#0000, 16#0800, "US"} => 16#0101,
	     {16#0000, 16#0900, "US"} => 16#0000},
    Data = wolfpacs_data_elements:encode(Flow, Strategy, Info),
    NbBytes = byte_size(Data),
    Header = wolfpacs_data_element:encode(Flow, Strategy, 0, 0, "UL", NbBytes),
    <<Header/binary, Data/binary>>.

%%==============================================================================
%% Private
%%==============================================================================


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    RQID = 1,
    UID = <<"1.2.3.4">>,
    Correct = <<0, 0, 0, 0,
		4, 0, 0, 0,
		56, 0, 0, 0,
		0,
		0, 2, 0, 8, 0,
		0, 0,
		49, 46, 50, 46, 51, 46, 52, 0,
		0, 0, 0, 1, 2, 0, 0, 0,
		48, 128, 0, 0, 32, 1, 2,
		0, 0, 0, 1, 0, 0, 0, 0, 8, 2,
		0, 0, 0, 1, 1, 0, 0, 0, 9, 2, 0, 0, 0, 0, 0>>,
    [ ?_assertEqual(encode(Flow, {implicit, little}, UID, RQID), Correct)
    , ?_assertEqual(encode(Flow, {explicit, little}, UID, RQID), Correct)
    ].
