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
%% @doc C Store Req (RQ)
%%
%% <table>
%%   <thead>
%%     <tr>
%%       <th>Message Field</th>
%%       <th>Tag</th>
%%       <th>VR</th>
%%       <th>VM</th>
%%       <th>>Description of Field</th>
%%     </tr>
%%   </thead>
%%   <tbody>
%%     <tr>
%%       <td>Command Group Length</td>
%%       <td>(0000,0000)</td>
%%       <td>UL</td>
%%       <td>1</td>
%%       <td>The even number of bytes from the end of the value field
%%           to the beginning of the next group.</td>
%%     </tr>
%%     <tr>
%%       <td>Affected SOP Class UID</td>
%%       <td>(0000,0002)</td>
%%       <td>UI</td>
%%       <td>1</td>
%%       <td>SOP Class UID of the SOP Instance to be stored.</td>
%%     </tr>
%%     <tr>
%%       <td>Command Field</td>
%%       <td>(0000,0100)</td>
%%       <td>US</td>
%%       <td>1</td>
%%       <td>This field distinguishes the DIMSE‑C operation conveyed by this Message.
%%           The value of this field shall be set to 0001H for the C-STORE-RQ Message.</td>
%%     </tr>
%%     <tr>
%%       <td>Message ID</td>
%%       <td>(0000,0110)</td>
%%       <td>US</td>
%%       <td>1</td>
%%       <td>Implementation-specific value. It distinguishes this Message from other Messages.</td>
%%     </tr>
%%     <tr>
%%       <td>Priority</td>
%%       <td>(0000,0700)</td>
%%       <td>US</td>
%%       <td>1</td>
%%       <td>The priority shall be set to one of the following values:
%%           LOW = 0002H MEDIUM = 0000H HIGH = 0001H</td>
%%     </tr>
%%     <tr>
%%       <td>Command Data Set Type</td>
%%       <td>(0000,0800)</td>
%%       <td>US</td>
%%       <td>1</td>
%%       <td>This field indicates that a Data Set is present in the Message.
%%           It shall be set to any value other than 0101H (Null).</td>
%%     </tr>
%%     <tr>
%%       <td>Affected SOP Instance UID</td>
%%       <td>(0000,1000)</td>
%%       <td>UI</td>
%%       <td>1</td>
%%       <td>Contains the UID of the SOP Instance to be stored.</td>
%%     </tr>
%%     <tr>
%%       <td>Move Originator Application Entity Title</td>
%%       <td>(0000,1030)</td>
%%       <td>AE</td>
%%       <td>1</td>
%%       <td>Contains the DICOM AE Title of the DICOM AE that invoked the C-MOVE operation
%%           from which this C-STORE sub-operation is being performed.</td>
%%     </tr>
%%     <tr>
%%       <td>Move Originator Message ID</td>
%%       <td>(0000,1031)</td>
%%       <td>US</td>
%%       <td>1</td>
%%       <td>Contains the Message ID (0000,0110) of the C-MOVE-RQ Message from which
%%           this C-STORE sub-operations is being performed.</td>
%%     </tr>
%%   </tbody>
%% </table>
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_c_store_rq).
-export([encode/3,
	 encode/4,
	 decode/3]).

encode(Flow, Strategy, ClassUID, InstanceUID) ->
    DataSet = #{{0,   2} => ClassUID,
		{0, 256} => 1,
		{0, 272} => 1,
		{0, 1792} => 0,
		{0, 2048} => 1,
		{0, 4096} => InstanceUID
	       },
    encode(Flow, Strategy, DataSet).

encode(Flow, Strategy, DataSet) ->
    Encoded = wolfpacs_data_elements:encode(Flow, Strategy, DataSet),
    Length = byte_size(Encoded),
    Header = wolfpacs_data_element:encode(Flow, Strategy, 0, 0, "UL", Length),
    <<Header/binary,
      Encoded/binary>>.

decode(Flow, Strategy, Data) ->
    wolfpacs_data_elements:decode(Flow, Strategy, Data).

%%==============================================================================
%% Test
%%==============================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(CLASS_UID, <<"1.2.840.10008.5.1.4.1.1.2">>).
-define(INSTANCE_UID, <<"2.16.840.1.113662.2.1.4519.41582.4105152.419990505.410523251">>).

encoded() ->
    Log =
	"0000 0000 0400 0000 8e00 0000 0000 0200
         1a00 0000 312e 322e 3834 302e 3130 3030
         382e 352e 312e 342e 312e 312e 3200 0000
         0001 0200 0000 0100 0000 1001 0200 0000
         0100 0000 0007 0200 0000 0000 0000 0008
         0200 0000 0100 0000 0010 3c00 0000 322e
         3136 2e38 3430 2e31 2e31 3133 3636 322e
         322e 312e 3435 3139 2e34 3135 3832 2e34
         3130 3531 3532 2e34 3139 3939 3035 3035
         2e34 3130 3532 3332 3531",
    wolfpacs_utils:hexl_log_to_binary(Log).

decode_test_() ->
    Encoded = encoded(),
    {ok, Rq, <<>>} = decode(no_flow, {explicit, little}, Encoded),
    Get = fun(E) -> maps:get({0, E}, Rq) end,

    [ ?_assertEqual(Get(2), ?CLASS_UID)
    , ?_assertEqual(Get(4096), ?INSTANCE_UID)
    ].

encode_test() ->
    Correct = encoded(),
    Encoded = encode(no_flow, {explicit, little}, ?CLASS_UID, ?INSTANCE_UID),
    ?assertEqual(Encoded, Correct).

-endif.
