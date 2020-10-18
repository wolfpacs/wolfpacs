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
