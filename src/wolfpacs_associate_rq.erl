%%%-------------------------------------------------------------------
%% @doc Associate Request (RQ)
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_associate_rq).
-export([encode/8,
	 decode/1]).

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
decode(OrgData) ->
    lager:warning("error: decode"),
    lager:warning("~p", [OrgData]),
    {error, OrgData}.

%%==============================================================================
%% Private
%%==============================================================================

decode_called_and_calling(OrgData, <<_:16, _:16, CalledAE:128/bitstring, CallingAE:128/bitstring, R:256/bitstring, Data/binary>>) ->
    MaybeVariableItems = wolfpacs_variable_items_request:decode(Data),
    decode_variable_items(OrgData, CalledAE, CallingAE, R, MaybeVariableItems);
decode_called_and_calling(OrgData, _) ->
    lager:warning("error: decode_called_and_calling"),
    {error, OrgData}.

decode_variable_items(_OrgData, CalledAE, CallingAE, R, {ok,
							 Contexts,
							 MaxSize, Class, VersionName, Rest}) ->
    {ok, CalledAE, CallingAE, R, Contexts, MaxSize, Class, VersionName, Rest};
decode_variable_items(OrgData, _, _, _, _) ->
    lager:warning("[associate_rq] error: decode variable items"),
    {error, OrgData}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_echoscu_test() ->
    PrCID = 1,
    AbstractSyntax = wolfpacs_sop:verification(),
    TransferSyntax = [wolfpacs_transfer_syntax:implicit_vr_little_endian()],
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
    AbstractSyntax = wolfpacs_sop:verification(),
    TransferSyntax = [wolfpacs_transfer_syntax:implicit_vr_little_endian()],
    CalledAE  = <<"ANY-SCP         ">>,
    CallingAE = <<"bbbbbb          ">>,
    R = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Encoded0 = encode(CalledAE, CallingAE, PrCID, AbstractSyntax, TransferSyntax, MaxPDUSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4>>,

    [?_assertEqual(decode(Encoded0), {ok, CalledAE, CallingAE, R,
				      [{PrCID, AbstractSyntax, TransferSyntax}],
				      MaxPDUSize, Class, VersionName,
				      <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, CalledAE, CallingAE, R,
				      [{PrCID, AbstractSyntax, TransferSyntax}],
				      MaxPDUSize, Class, VersionName,
				      <<42>>}),
     ?_assertEqual(decode(Incorrect0), {error,  Incorrect0}),
     ?_assertEqual(decode(Incorrect1), {error,  Incorrect1})
    ].
