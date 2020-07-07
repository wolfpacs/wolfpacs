%%%-------------------------------------------------------------------
%% @doc Variable Items Requests.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_variable_items_request).
-export([encode/4,
	 decode/1]).
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").

encode(Contexts, MaxPDUSize, Class, VersionName) ->
    Fixed = <<"1.2.840.10008.3.1.1.1">>,
    ApplicationContext = wolfpacs_application_context_name:encode(no_flow, Fixed),
    PresentationContext = wolfpacs_presentation_contexts_request:encode(Contexts),
    UserInformation = wolfpacs_user_information:encode(MaxPDUSize, Class, VersionName),
    <<ApplicationContext/binary,
      PresentationContext/binary,
      UserInformation/binary>>.

decode(Data) ->
    case wolfpacs_application_context_name:decode(no_flow, Data) of
	{ok, _, Rest} ->
	    decode_context(Rest);
	_ ->
	    error
    end.

%%==============================================================================
%% Private
%%==============================================================================

decode_context(Data) ->
    MaybePresentationContext = wolfpacs_presentation_contexts_request:decode(Data),
    decode_presentation_context(MaybePresentationContext).

decode_presentation_context({ok, Contexts, Data}) ->
    MaybeUserInformation = wolfpacs_user_information:decode(Data),
    decode_user_information(Contexts, MaybeUserInformation);
decode_presentation_context(_) ->
    error.

decode_user_information(Contexts, {ok, MaxSize, Class, VersionName, Rest}) ->
    {ok, Contexts, MaxSize, Class, VersionName, Rest};
decode_user_information(_,  _) ->
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    PrCID = 42,
    AbstractSyntax = ?VERIFICATION,
    TransferSyntax = [?IMPLICIT_LITTLE_ENDIAN,
		      ?EXPLICIT_LITTLE_ENDIAN,
		      ?EXPLICIT_BIG_ENDIAN],
    Contexts = [{PrCID, AbstractSyntax, TransferSyntax}],
    MaxPDUSize = 65536,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,
    Encoded0 = encode(Contexts, MaxPDUSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = <<1,2,3,4>>,

    [ ?_assertEqual(decode(Encoded0), {ok, Contexts, MaxPDUSize, Class, VersionName, <<>>})
    , ?_assertEqual(decode(Encoded1), {ok, Contexts, MaxPDUSize, Class, VersionName, <<42>>})
    , ?_assertEqual(decode(Incorrect0), error)
    , ?_assertEqual(decode(Incorrect1), error)
    , ?_assertEqual(decode(Incorrect2), error)
    ].
