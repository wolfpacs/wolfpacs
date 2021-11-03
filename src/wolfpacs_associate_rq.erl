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
%% @doc Associate Request (RQ)
%%
%% <table>
%%   <thead>
%%     <tr>
%%       <th>PDU bytes</th>
%%       <th>Field name</th>
%%       <th>Description of field</th>
%%     </tr>
%%   </thead>
%%   <tbody>
%%     <tr>
%%       <td>1</td>
%%       <td>PDU-type</td>
%%       <td>01H</td>
%%     </tr>
%%     <tr>
%%       <td>2</td>
%%       <td>Reserved</td>
%%       <td>This reserved field shall be sent with a value 00H but not tested to this value when received.</td>
%%     </tr>
%%     <tr>
%%       <td>3-6</td>
%%       <td>PDU-length</td>
%%       <td>This PDU-length shall be the number of bytes from the first byte of the following field to the last byte of the variable field. It shall be encoded as an unsigned binary number</td>
%%     </tr>
%%     <tr>
%%       <td>7-8</td>
%%       <td>Protocol-version</td>
%%       <td>This two byte field shall use one bit to identify each version of the DICOM UL protocol supported by the calling end-system. This is Version 1 and shall be identified with bit 0 set. A receiver of this PDU implementing only this version of the DICOM UL protocol shall only test that bit 0 is set.</td>
%%     </tr>
%%     <tr>
%%       <td>9-10</td>
%%       <td>Reserved</td>
%%       <td>This reserved field shall be sent with a value 0000H but not tested to this value when received.</td>
%%     </tr>
%%     <tr>
%%       <td>11-26</td>
%%       <td>Called-AE-title</td>
%%       <td>Destination DICOM Application Name. It shall be encoded as 16 characters as defined by the ISO 646:1990-Basic G0 Set with leading and trailing spaces (20H) being non-significant. The value made of 16 spaces (20H) meaning "no Application Name specified" shall not be used. For a complete description of the use of this field, see Section 7.1.1.4.</td>
%%     </tr>
%%     <tr>
%%       <td>27-42</td>
%%       <td>Calling-AE-title</td>
%%       <td>Source DICOM Application Name. It shall be encoded as 16 characters as defined by the ISO 646:1990-Basic G0 Set with leading and trailing spaces (20H) being non-significant. The value made of 16 spaces (20H) meaning "no Application Name specified" shall not be used. For a complete description of the use of this field, see Section 7.1.1.3.</td>
%%     </tr>
%%     <tr>
%%       <td>43-74</td>
%%       <td>Reserved</td>
%%       <td>This reserved field shall be sent with a value 00H for all bytes but not tested to this value when received</td>
%%     </tr>
%%     <tr>
%%       <td>75-xxx</td>
%%       <td>Variable items</td>
%%       <td>This variable field shall contain the following items: one Application Context Item, one or more Presentation Context Items and one User Information Item. For a complete description of the use of these items see Section 7.1.1.2, Section 7.1.1.13, and Section 7.1.1.6.</td>
%%     </tr>
%%   </tbody>
%% </table>
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_associate_rq).
-export([encode/7,
	 decode/2]).
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").
-include("wolfpacs_types.hrl").

encode(_Flow, CalledAE, CallingAE, Contexts, MaxPDUSize, Class, VersionName) ->
    VariableItems = wolfpacs_variable_items_request:encode(Contexts,
							   MaxPDUSize,
							   Class,
							   VersionName),
    CalledAE16 = wolfpacs_vr_utils:exact(CalledAE, 16, " "),
    CallingAE16 = wolfpacs_vr_utils:exact(CallingAE, 16, " "),

    Data = <<1:16,  %% Protocol Version
	     0:16,  %% Reserved
	     CalledAE16/binary,
	     CallingAE16/binary,
	     0:256,
	     VariableItems/binary>>,

    Length = byte_size(Data),

    <<16#1, 0, Length:32, Data/binary>>.

decode(Flow, <<16#1, _, _Length:32, Data/binary>>) ->
    decode_called_and_calling(Flow, Data);
decode(Flow, <<_PV, _/binary>>) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error;
decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "no data"),
    error.

%%==============================================================================
%% Private
%%==============================================================================

decode_called_and_calling(Flow, <<_:16, _:16, CalledAE:128/bitstring, CallingAE:128/bitstring, R:256/bitstring, Data/binary>>) ->
    MaybeVariableItems = wolfpacs_variable_items_request:decode(Data),
    decode_variable_items(Flow, CalledAE, CallingAE, R, MaybeVariableItems);
decode_called_and_calling(Flow, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode called and calling"),
    error.

decode_variable_items(_Flow, CalledAE, CallingAE, R, {ok,
						      Contexts,
						      MaxSize, Class, VersionName, Rest}) ->
    {ok, CalledAE, CallingAE, R, Contexts, MaxSize, Class, VersionName, Rest};
decode_variable_items(Flow, _, _, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode variable items"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

example_encoded() ->
    Log = "\
D:   01  00  00  00  00  cd  00  01  00  00  43  41  4c  4c  49  4e
D:   47  38  41  42  31  32  33  34  35  36  31  32  33  34  35  36
D:   37  38  39  30  31  32  33  34  35  36  00  00  00  00  00  00
D:   00  00  00  00  00  00  00  00  00  00  00  00  00  00  00  00
D:   00  00  00  00  00  00  00  00  00  00  10  00  00  15  31  2e
D:   32  2e  38  34  30  2e  31  30  30  30  38  2e  33  2e  31  2e
D:   31  2e  31  20  00  00  2e  01  00  ff  00  30  00  00  11  31
D:   2e  32  2e  38  34  30  2e  31  30  30  30  38  2e  31  2e  31
D:   40  00  00  11  31  2e  32  2e  38  34  30  2e  31  30  30  30
D:   38  2e  31  2e  32  50  00  00  3a  51  00  00  04  00  00  40
D:   00  52  00  00  1b  31  2e  32  2e  32  37  36  2e  30  2e  37
D:   32  33  30  30  31  30  2e  33  2e  30  2e  33  2e  36  2e  34
D:   55  00  00  0f  4f  46  46  49  53  5f  44  43  4d  54  4b  5f
D:   33  36  34",
    wolfpacs_utils:log_to_binary(Log).

decode_test_() ->
    Encoded = example_encoded(),
    {ok,
     CalledAE,
     CallingAE,
     _R,
     Contexts,
     MaxSize,
     Class,
     VersionName,
     Rest} = decode(no_flow, Encoded),

    [ ?_assertEqual(CalledAE, <<"CALLING8AB123456">>)
    , ?_assertEqual(CallingAE, <<"1234567890123456">>)
    , ?_assertEqual(Contexts, [{1,
				<<"1.2.840.10008.1.1">>,
				[<<"1.2.840.10008.1.2">>]}])
    , ?_assertEqual(MaxSize, 16384)
    , ?_assertEqual(Class, <<"1.2.276.0.7230010.3.0.3.6.4">>)
    , ?_assertEqual(VersionName, <<"OFFIS_DCMTK_364">>)
    , ?_assertEqual(Rest, <<>>)
    ].

encode_test() ->
    CalledAE = <<"CALLING8AB123456">>,
    CallingAE = <<"1234567890123456">>,

    Contexts = [{1,
		<<"1.2.840.10008.1.1">>,
		[<<"1.2.840.10008.1.2">>]}],

    MaxSize =16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Correct = example_encoded(),
    Encoded = encode(no_flow, CalledAE, CallingAE, Contexts, MaxSize, Class, VersionName),

    ?_assertEqual(Encoded, Correct).

incorrect_headers_test_() ->
    [ ?_assertEqual(decode(no_flow, <<2, 3, 4, 5>>), error)
    , ?_assertEqual(decode(no_flow, <<2>>), error)
    , ?_assertEqual(decode(no_flow, <<>>), error)
    ].

encode_decode_test_() ->
    CalledAE = <<"CALLING8AB123456">>,
    CallingAE = <<"1234567890123456">>,
    Contexts = [{1,
		 <<"1.2.840.10008.1.1">>,
		 [<<"1.2.840.10008.1.2">>]}],
    MaxSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,
    R = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,

    Encoded0 = encode(no_flow, CalledAE, CallingAE, Contexts, MaxSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = wolfpacs_utils:clear_byte(Encoded0, 20),
    Incorrect3 = wolfpacs_utils:clear_odd(Encoded0),

    [ ?_assertEqual(decode(no_flow, Encoded0),
		    {ok, CalledAE, CallingAE, R, Contexts, MaxSize, Class, VersionName, <<>>})
    , ?_assertEqual(decode(no_flow, Encoded1),
		    {ok, CalledAE, CallingAE, R, Contexts, MaxSize, Class, VersionName, <<42>>})
    , ?_assertEqual(decode(no_flow, Incorrect0), error)
    , ?_assertEqual(decode(no_flow, Incorrect1), error)
    , ?_assertEqual(decode(no_flow, Incorrect2), error)
    , ?_assertEqual(decode(no_flow, Incorrect3), error)
    ].

broken_encoded_decode_test() ->
    CalledAE = <<"CALLING8AB123456">>,
    CallingAE = <<"1234567890123456">>,
    Contexts = [{1,
		 <<"1.2.840.10008.1.1">>,
		 [<<"1.2.840.10008.1.2">>]}],
    MaxSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Encoded = encode(no_flow, CalledAE, CallingAE, Contexts, MaxSize, Class, VersionName),
    Length = byte_size(Encoded),

    {ok, Broken, _} = wolfpacs_utils:split(Encoded, Length div 2),

    ?assertEqual(decode(no_flow, Broken), error).
