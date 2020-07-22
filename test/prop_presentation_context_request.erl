-module(prop_presentation_context_request).
-include_lib("proper/include/proper.hrl").
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(_, term(),
	    begin
		PrCID = 42,
		AbstractSyntax = ?VERIFICATION,
		TransferSyntax = [?IMPLICIT_LITTLE_ENDIAN,
				  ?EXPLICIT_LITTLE_ENDIAN,
				  ?EXPLICIT_BIG_ENDIAN],
		Encoded = wolfpacs_presentation_context_request:encode(no_flow, PrCID, AbstractSyntax, TransferSyntax),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.20),
		case wolfpacs_presentation_context_request:decode(no_flow, Corrupt) of
		    {ok, _, _, _, _} ->
			true;
		    error ->
			true;
		    What  ->
			lager:warning("~p", [What]),
			false
		end
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
