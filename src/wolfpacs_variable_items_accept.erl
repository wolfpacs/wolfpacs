-module(wolfpacs_variable_items_accept).
-export([encode/5,
	 decode/1]).

-spec encode(byte(), binary(), non_neg_integer(), binary(), binary()) -> binary().
encode(PrCID, TransferSyntax, MaxPDUSize, Class, VersionName) ->
    Fixed = <<"1.2.840.10008.3.1.1.1">>,
    ApplicationContextName = wolfpacs_application_context_name:encode(Fixed),
    PresentationContext = wolfpacs_presentation_context_accept:encode(PrCID, TransferSyntax),
    UserInformation = wolfpacs_user_information:encode(MaxPDUSize, Class, VersionName),
    Payload = <<ApplicationContextName/binary,
		PresentationContext/binary,
		UserInformation/binary>>,
    Payload.

%% TODO
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Private
%%==============================================================================

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

storescp_echoscu_test() ->
    PrCID = 1,
    TransferSyntax = <<"1.2.840.10008.1.2">>,
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,
    Data = wolfpacs_utils:log_to_binary("
                                             10  00  00  15  31  2e
D:   32  2e  38  34  30  2e  31  30  30  30  38  2e  33  2e  31  2e
D:   31  2e  31  21  00  00  19  01  00  00  00  40  00  00  11  31
D:   2e  32  2e  38  34  30  2e  31  30  30  30  38  2e  31  2e  32
D:   50  00  00  3a  51  00  00  04  00  00  40  00  52  00  00  1b
D:   31  2e  32  2e  32  37  36  2e  30  2e  37  32  33  30  30  31
D:   30  2e  33  2e  30  2e  33  2e  36  2e  34  55  00  00  0f  4f
D:   46  46  49  53  5f  44  43  4d  54  4b  5f  33  36  34
"),
    ?assertEqual(encode(PrCID, TransferSyntax, MaxPDUSize, Class, VersionName), Data).
