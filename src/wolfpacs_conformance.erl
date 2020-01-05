%%%-------------------------------------------------------------------
%% @doc Conformance.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_conformance).
-export([supported/1]).

%%-------------------------------------------------------------------
%% @doc Supported.
%%
%% @end
%%-------------------------------------------------------------------
-spec supported(list({integer(), binary(), list(binary())})) -> {ok, list({integer(), binary()})}.
supported(PresentationContexts) ->
    supported(PresentationContexts, []).

%%==============================================================================
%% Private
%%==============================================================================

supported([], Acc) ->
    {ok, lists:reverse(Acc)};
supported([{PrCID, AbstractSyntax, TransferSyntexes}|Contexts], Acc) ->
    case supported_abstract_syntax(AbstractSyntax, TransferSyntexes) of
	no ->
	    supported(Contexts, Acc);
	{yes, TransferSyntex} ->
	    supported(Contexts, [{PrCID, TransferSyntex}|Acc])
    end.

supported_abstract_syntax(_AbstractSyntax, []) ->
    no;
supported_abstract_syntax(AbstractSyntax, [TransferSyntax|TransferSyntaxes]) ->
    case supported_transfer_syntax(AbstractSyntax, TransferSyntax) of
	yes ->
	    {yes, TransferSyntax};
	no  ->
	    supported_abstract_syntax(AbstractSyntax, TransferSyntaxes)
    end.

supported_transfer_syntax(<<"1.2.840.10008.1.1">>, <<"1.2.840.10008.1.2">>) ->
    yes;
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

    ?assertEqual(supported(Contexts), {ok, Correct}).
