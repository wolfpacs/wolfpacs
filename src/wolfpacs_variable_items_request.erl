-module(wolfpacs_variable_items_request).
-export([encode/6,
	 decode/1]).
-import(wolfpacs_utils, [drop_last_byte/1]).

encode(PrCID, AbstractSyntax, TransferSyntax, MaxPDUSize, Class, VersionName) ->
    PresentationContext = wolfpacs_presentation_context_request:encode(PrCID, AbstractSyntax, TransferSyntax),
    UserInformation = wolfpacs_user_information:encode(MaxPDUSize, Class, VersionName),
    Payload = <<"1.2.840.10008.3.1.1.1",
		PresentationContext/binary,
		UserInformation/binary>>,
    encode(Payload).

decode(OrgData = <<16#10, 0, _Size:16, "1.2.840.10008.3.1.1.1", Data/binary>>) ->
    MaybePresentationContext = wolfpacs_presentation_contexts_request:decode(Data),
    decode_presentation_context(OrgData, MaybePresentationContext);
decode(Data) ->
    {error, Data}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

encode(Payload) ->
    Length = byte_size(Payload),
    <<16#10,
      0,
      Length:16,
      Payload/binary>>.

decode_presentation_context(OrgData, {ok, Contexts, Data}) ->
    MaybeUserInformation = wolfpacs_user_information:decode(Data),
    decode_user_information(OrgData, Contexts, MaybeUserInformation);
decode_presentation_context(OrgData, _) ->
    {error, OrgData}.

decode_user_information(_OrgData, Contexts, {ok, MaxSize, Class, VersionName, Rest}) ->
    {ok, Contexts, MaxSize, Class, VersionName, Rest};
decode_user_information(OrgData, _,  _) ->
    {error, OrgData}.

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    PrCID = 42,
    AbstractSyntax = wolfpacs_sop:verification(),
    TransferSyntax = [wolfpacs_transfer_syntax:implicit_vr_little_endian(),
		      wolfpacs_transfer_syntax:explicit_vr_little_endian(),
		      wolfpacs_transfer_syntax:explicit_vr_big_endian()],
    MaxPDUSize = 65536,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,
    Encoded0 = encode(PrCID, AbstractSyntax, TransferSyntax, MaxPDUSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = drop_last_byte(Encoded0),
    Incorrect1 = <<1,2,3,4>>,

    [ ?_assertEqual(decode(Encoded0), {ok, [{PrCID, AbstractSyntax, TransferSyntax}],
				       MaxPDUSize, Class, VersionName, <<>>}),
      ?_assertEqual(decode(Encoded1), {ok, [{PrCID, AbstractSyntax, TransferSyntax}],
				       MaxPDUSize, Class, VersionName, <<42>>}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
      ?_assertEqual(decode(Incorrect1), {error, Incorrect1}) ].
