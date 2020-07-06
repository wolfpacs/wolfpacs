%%%-------------------------------------------------------------------
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
-export([encode/8,
	 decode/1]).
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").

encode(CalledAE, CallingAE, PrCID, AbstractSyntax, TransferSyntax, MaxPDUSize, Class, VersionName) ->
    VariableItems = wolfpacs_variable_items_request:encode(PrCID,
							   AbstractSyntax,
							   TransferSyntax,
							   MaxPDUSize,
							   Class,
							   VersionName),
    CalledAE16 = wolfpacs_vr_utils:exact_binary(CalledAE, 16),
    CallingAE16 = wolfpacs_vr_utils:exact_binary(CallingAE, 16),

    Data = <<1:16,  %% Protocol Version
	     0:16,  %% Reserved
	     CalledAE16/binary,
	     CallingAE16/binary,
	     0:256,
	     VariableItems/binary>>,

    Length = byte_size(Data),

    <<16#1, 0, Length:32, Data/binary>>.

decode(OrgData = <<16#1, _, _Length:32, Data/binary>>) ->
    decode_called_and_calling(OrgData, Data);
decode(OrgData = <<PV, _/binary>>) ->
    {error, OrgData, ["incorrect header", PV]};
decode(OrgData) ->
    {error, OrgData, ["no data"]}.

%%==============================================================================
%% Private
%%==============================================================================

decode_called_and_calling(OrgData, <<_:16, _:16, CalledAE:128/bitstring, CallingAE:128/bitstring, R:256/bitstring, Data/binary>>) ->
    MaybeVariableItems = wolfpacs_variable_items_request:decode(Data),
    decode_variable_items(OrgData, CalledAE, CallingAE, R, MaybeVariableItems);
decode_called_and_calling(OrgData, _) ->
    {error, OrgData, ["unable to decode called and calling"]}.

decode_variable_items(_OrgData, CalledAE, CallingAE, R, {ok,
							 Contexts,
							 MaxSize, Class, VersionName, Rest}) ->
    {ok, CalledAE, CallingAE, R, Contexts, MaxSize, Class, VersionName, Rest};
decode_variable_items(OrgData, _, _, _, _) ->
    {error, OrgData, ["unable to decode variable items"]}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_echoscu_test() ->
    PrCID = 1,
    AbstractSyntax = ?VERIFICATION,
    TransferSyntax = [?IMPLICIT_LITTLE_ENDIAN],
    CalledAE  = <<"ANY-SCP         ">>,
    CallingAE = <<"bbbbbb          ">>,
    R = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
    MaxSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Correct = <<1,
		0,0,0,0,

		205,
		0,1,0,0,

		65,78,89,45,83,67,80,32,32,32,32,32,32,32,32,32,
		98,98,98,98,98,98,32,32,32,32,32,32,32,32,32,32,

		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,

		%% Variable Items start here
		16,0,0,21,
		49,46,50,46,56,52,48,46,49,48,48,48,56,46,51,46,49,46,49,46,49,

		%% Presentation context
		32,0,0,46,1,0,255,0,

		%%
		48,0,0,17,
		49,46,50,46,56,52,48,46,49,48,48,48,56,46,49,46,49,

		64,0,0,17,
		49,46,50,46,56,52,48,46,49,48,48,48,56,46,49,46,50,

		%% 50H = 80 => User Information Item Fields
		80,0,0,58,
		81,0,0,4,
		%% Max length
		0,0,64,0,
		82,0,0,27,
		%% "1.2.276.0.7230010.3.0.3.6.4"
		49,46,50,46,50,55,54,46,48,46,55,50,51,48,48,49,48,46,51,46,48,46,51,46,54,46,52,

		85,0,0,15,
		%% "OFFIS_DCMTK_364"
		79,70,70,73,83,95,68,67,77,84,75,95,51,54,52>>,
    ?assertEqual(decode(Correct),
		 {ok, CalledAE, CallingAE, R,
		  [{PrCID, AbstractSyntax, TransferSyntax}],
		  MaxSize, Class, VersionName,
		  <<>>}).

encode_decode_test_() ->
    PrCID = 1,
    AbstractSyntax = ?VERIFICATION,
    TransferSyntax = [?IMPLICIT_LITTLE_ENDIAN],
    CalledAE  = <<"ANY-SCP         ">>,
    CallingAE = <<"bbbbbb          ">>,
    R = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Encoded0 = encode(CalledAE, CallingAE, PrCID, AbstractSyntax, TransferSyntax, MaxPDUSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4>>,
    Incorrect3 = binary:replace(Encoded0, <<"1.2.840">>, <<>>),
    Incorrect4 = <<1, 0, 0:32, 1>>,
    Incorrect5 = <<>>,

    [?_assertEqual(decode(Encoded0), {ok, CalledAE, CallingAE, R,
				      [{PrCID, AbstractSyntax, TransferSyntax}],
				      MaxPDUSize, Class, VersionName,
				      <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, CalledAE, CallingAE, R,
				      [{PrCID, AbstractSyntax, TransferSyntax}],
				      MaxPDUSize, Class, VersionName,
				      <<42>>}),
     ?_assertEqual(decode(Incorrect0), {error,  Incorrect0, ["unable to decode variable items"]}),
     ?_assertEqual(decode(Incorrect1), {error,  Incorrect1, ["incorrect header", 0]}),
     ?_assertEqual(decode(Incorrect2), {error,  Incorrect2, ["incorrect header", 1]}),
     ?_assertEqual(decode(Incorrect3), {error,  Incorrect3, ["unable to decode variable items"]}),
     ?_assertEqual(decode(Incorrect4), {error,  Incorrect4, ["unable to decode called and calling"]}),
     ?_assertEqual(decode(Incorrect5), {error,  Incorrect5, ["no data"]})
    ].

decode_test_() ->
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
D:   33  36  34 ff",
    Encoded = wolfpacs_utils:log_to_binary(Log),
    {ok,
     CalledAE,
     CallingAE,
     _R,
     Contexts,
     MaxSize,
     Class,
     VersionName,
     Rest} = decode(Encoded),

    [ ?_assertEqual(CalledAE, <<"CALLING8AB123456">>)
    , ?_assertEqual(CallingAE, <<"1234567890123456">>)
    , ?_assertEqual(Contexts, [{1,
				<<"1.2.840.10008.1.1">>,
				[<<"1.2.840.10008.1.2">>]}])
    , ?_assertEqual(MaxSize, 16384)
    , ?_assertEqual(Class, <<"1.2.276.0.7230010.3.0.3.6.4">>)
    , ?_assertEqual(VersionName, <<"OFFIS_DCMTK_364">>)
    , ?_assertEqual(Rest, <<16#ff>>)
    ].
