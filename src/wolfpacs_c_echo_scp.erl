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
