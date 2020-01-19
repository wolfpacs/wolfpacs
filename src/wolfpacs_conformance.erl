%%%-------------------------------------------------------------------
%% @doc Conformance.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_conformance).
-export([supported/1]).

-define(VERIFICATION, <<"1.2.840.10008.1.1">>).
-define(SECONDARY_CAPTURE, <<"1.2.840.10008.5.1.4.1.1.7">>).
-define(CT_IMAGE_STORAGE, <<"1.2.840.10008.5.1.4.1.1.2">>).

%%-------------------------------------------------------------------
%% @doc Supported.
%%
%% @end
%%-------------------------------------------------------------------
-spec supported(list({integer(), binary(), list(binary())})) -> {ok, list({integer(), binary(), map()})}.
supported(PresentationContexts) ->
    supported(PresentationContexts, [], #{}).

%%==============================================================================
%% Private
%%==============================================================================

supported([], Acc, Map) ->
    {ok, lists:reverse(Acc), Map};
supported([{PrCID, AbstractSyntax, TransferSyntexes}|Contexts], Acc, Map) ->
    case supported_abstract_syntax(AbstractSyntax, TransferSyntexes) of
	no ->
	    supported(Contexts, Acc, Map);
	{yes, TransferSyntax, ConformanceTag} ->
	    supported(Contexts, [{PrCID, TransferSyntax}|Acc], Map#{PrCID => ConformanceTag})
    end.

supported_abstract_syntax(_AbstractSyntax, []) ->
    no;
supported_abstract_syntax(AbstractSyntax, [TransferSyntax|TransferSyntaxes]) ->
    case supported_transfer_syntax(AbstractSyntax, TransferSyntax) of
	no  ->
	    supported_abstract_syntax(AbstractSyntax, TransferSyntaxes);
	ConformanceTag ->
	    {yes, TransferSyntax, ConformanceTag}
    end.

supported_transfer_syntax(?VERIFICATION, <<"1.2.840.10008.1.2">>) ->
    verification_explicit_little;
supported_transfer_syntax(?SECONDARY_CAPTURE, <<"1.2.840.10008.1.2">>) ->
    secondary_capture_explicit_little;
supported_transfer_syntax(?CT_IMAGE_STORAGE, <<"1.2.840.10008.1.2">>) ->
    ct_image_storage_explicit_little;
supported_transfer_syntax(_, _) ->
    no.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

supported_test() ->
    Verification = wolfpacs_sop:verification(),
    PrintJob = <<"1.2.840.10008.5.1.1.14">>,

    ImplicitLittle = wolfpacs_transfer_syntax:implicit_vr_little_endian(),
    ExplicitLittle = wolfpacs_transfer_syntax:explicit_vr_little_endian(),

    Contexts = [{42, Verification, [ImplicitLittle]},
		{43, Verification, [ExplicitLittle]},
		{44, Verification, [ExplicitLittle, ImplicitLittle]},

		{45, PrintJob, [ImplicitLittle]},
		{46, PrintJob, [ExplicitLittle]},
		{47, PrintJob, [ExplicitLittle, ImplicitLittle]}],

    Correct = [{42, ImplicitLittle},
	       {44, ImplicitLittle}],

    CorrectMap = #{42 => verification_explicit_little,
		   44 => verification_explicit_little},

    ?assertEqual(supported(Contexts), {ok, Correct, CorrectMap}).
