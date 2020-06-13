%%%-------------------------------------------------------------------
%% @doc Conformance.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_conformance).
-export([supported/2,
	 transfer_syntax_to_strategy/1]).
-include("transfer_syntax.hrl").
-include("abstract_syntax.hrl").

%%-------------------------------------------------------------------
%% @doc Supported.
%%
%% @end
%%-------------------------------------------------------------------
supported(PresentationContexts, true) ->
    supported(PresentationContexts, [], #{});
supported(PresentationContexts, false) ->
    lager:warning("Only allow verification"),
    supported(lists:filter(fun only_keep_verification/1,
			   PresentationContexts),
	      true).

transfer_syntax_to_strategy(?IMPLICIT_LITTLE_ENDIAN) ->
    {implicit, little};
transfer_syntax_to_strategy(?EXPLICIT_LITTLE_ENDIAN) ->
    {explicit, little};
transfer_syntax_to_strategy(?EXPLICIT_BIG_ENDIAN) ->
    {explicit, big};
transfer_syntax_to_strategy(_) ->
    no_strategy_for_transfer_syntax.

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

supported_transfer_syntax(?VERIFICATION, ?IMPLICIT_LITTLE_ENDIAN) ->
    {verification, {implicit, little}};
supported_transfer_syntax(?VERIFICATION, ?EXPLICIT_LITTLE_ENDIAN) ->
    {verification, {explicit, little}};
supported_transfer_syntax(?VERIFICATION, ?EXPLICIT_BIG_ENDIAN) ->
    {verification, {explicit, big}};

supported_transfer_syntax(?SECONDARY_CAPTURE, ?IMPLICIT_LITTLE_ENDIAN) ->
    {secondary_capture, {implicit, little}};
supported_transfer_syntax(?SECONDARY_CAPTURE, ?EXPLICIT_LITTLE_ENDIAN) ->
    {secondary_capture, {explicit, little}};
supported_transfer_syntax(?SECONDARY_CAPTURE, ?EXPLICIT_BIG_ENDIAN) ->
    {secondary_capture, {explicit, big}};

supported_transfer_syntax(?CT_IMAGE_STORAGE, ?IMPLICIT_LITTLE_ENDIAN) ->
    {image_storage, {implicit, little}};
supported_transfer_syntax(?CT_IMAGE_STORAGE, ?EXPLICIT_LITTLE_ENDIAN) ->
    {image_storage, {explicit, little}};
supported_transfer_syntax(?CT_IMAGE_STORAGE, ?EXPLICIT_BIG_ENDIAN) ->
    {image_storage, {explicit, big}};

supported_transfer_syntax(?MR_IMAGE_STORAGE, ?IMPLICIT_LITTLE_ENDIAN) ->
    {image_storage, {implicit, little}};
supported_transfer_syntax(?MR_IMAGE_STORAGE, ?EXPLICIT_LITTLE_ENDIAN) ->
    {image_storage, {explicit, little}};
supported_transfer_syntax(?MR_IMAGE_STORAGE, ?EXPLICIT_BIG_ENDIAN) ->
    {image_storage, {explicit, big}};

supported_transfer_syntax(_, _) ->
    no.

only_keep_verification({_PrCID, ?VERIFICATION, _TransferSyntexes}) ->
    true;
only_keep_verification(_) ->
    false.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

supported_test() ->
    Contexts = [{42, ?VERIFICATION, [?IMPLICIT_LITTLE_ENDIAN]},
		{43, ?VERIFICATION, [?EXPLICIT_LITTLE_ENDIAN]},
		{44, ?VERIFICATION, [?EXPLICIT_LITTLE_ENDIAN, ?IMPLICIT_LITTLE_ENDIAN]},

		{45, ?PRINT_JOB, [?IMPLICIT_LITTLE_ENDIAN]},
		{46, ?PRINT_JOB, [?EXPLICIT_LITTLE_ENDIAN]},
		{47, ?PRINT_JOB, [?EXPLICIT_LITTLE_ENDIAN, ?IMPLICIT_LITTLE_ENDIAN]}],

    Correct = [{42, ?IMPLICIT_LITTLE_ENDIAN},
	       {43, ?EXPLICIT_LITTLE_ENDIAN},
	       {44, ?EXPLICIT_LITTLE_ENDIAN}],

    CorrectMap = #{42 => {verification, {implicit, little}},
		   43 => {verification, {explicit, little}},
		   44 => {verification, {explicit, little}}},

    Allowed = true,
    ?assertEqual(supported(Contexts, Allowed), {ok, Correct, CorrectMap}).

transfer_syntax_to_strategy_test() ->
    Correct = no_strategy_for_transfer_syntax,
    ?assertEqual(transfer_syntax_to_strategy(<<"1.2.3.4">>), Correct).
