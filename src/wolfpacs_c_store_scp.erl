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
%% @doc C Store SCP.
%%
%% https://www.dabsoft.ch/dicom/7/9.3.1/
%% (0000, 0000) UL Length
%% (0000, 0002) UI UID
%% (0000, 0100) US 8001H
%% (0000, 0120) US RQ ID
%% (0000, 0800) US 0101H
%% (0000, 0900) US 0000H (Success)
%% (0000, 1000) UI UID of Stored Instance
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_c_store_scp).
-export([encode/5]).

encode(Flow, Strategy, UID, RQID, StoredUID) ->
    Info = #{{16#0000, 16#0002, "UI"} => UID,
	     {16#0000, 16#0100, "US"} => 16#8001,
	     {16#0000, 16#0120, "US"} => RQID,
	     {16#0000, 16#0800, "US"} => 16#0101,
	     {16#0000, 16#0900, "US"} => 16#0000,
	     {16#0000, 16#1000, "UI"} => StoredUID},
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
