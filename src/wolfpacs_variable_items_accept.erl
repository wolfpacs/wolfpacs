%%%-------------------------------------------------------------------
%% @doc Variable Items Accept.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_variable_items_accept).
-export([encode/4,
	 decode/1]).

-spec encode(list({byte(), binary()}), non_neg_integer(), binary(), binary()) -> binary().
encode(SupportedContexts, MaxPDUSize, Class, VersionName) ->
    Fixed = <<"1.2.840.10008.3.1.1.1">>,
    ApplicationContextName = wolfpacs_application_context_name:encode(Fixed),
    PresentationContexts = wolfpacs_presentation_contexts_accept:encode(SupportedContexts),
    UserInformation = wolfpacs_user_information:encode(MaxPDUSize, Class, VersionName),
    Payload = <<ApplicationContextName/binary,
		PresentationContexts/binary,
		UserInformation/binary>>,
    Payload.

decode(Data) ->
    decode_with_name(Data, wolfpacs_application_context_name:decode(Data)).

%%==============================================================================
%% Private
%%==============================================================================

decode_with_name(OrgData, {error, _}) ->
    lager:warning("unable to decode_with_name"),
    {error, OrgData};
decode_with_name(OrgData, {ok, Name, Rest}) ->
    decode_with_accept(OrgData, Name, wolfpacs_presentation_contexts_accept:decode(Rest)).

decode_with_accept(OrgData, _, {error, _}) ->
    lager:warning("unable to decode_with_accept"),
    {error, OrgData};
decode_with_accept(OrgData, Name, {ok, Contexts, Rest}) ->
    MaybeUserInformation = wolfpacs_user_information:decode(Rest),
    decode_with_user_information(OrgData, Name, Contexts, MaybeUserInformation).

decode_with_user_information(OrgData, _Name, _Contexts, {error, _}) ->
    lager:warning("unable to decode_with_user_information"),
    {error, OrgData};
decode_with_user_information(_OrgData, Name, Contexts, {ok, MaxSize, ImplementationClass, VersionName, Rest}) ->
    {ok, Name, Contexts, MaxSize, ImplementationClass, VersionName, Rest}.

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
    ?assertEqual(encode([{PrCID, TransferSyntax}], MaxPDUSize, Class, VersionName), Data).

encode_decode_test_() ->
    Fixed = <<"1.2.840.10008.3.1.1.1">>,
    PrCID = 1,
    TransferSyntax = <<"1.2.840.10008.1.2">>,
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Encoded0 = encode([{PrCID, TransferSyntax}], MaxPDUSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,

    Correct0 = {ok, Fixed, [{PrCID, TransferSyntax}], MaxPDUSize, Class, VersionName, <<>>},
    Correct1 = {ok, Fixed, [{PrCID, TransferSyntax}], MaxPDUSize, Class, VersionName, <<42>>},

    [?_assertEqual(decode(Encoded0), Correct0),
     ?_assertEqual(decode(Encoded1), Correct1),
     ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
     ?_assertEqual(decode(Incorrect1), {error, Incorrect1})].
