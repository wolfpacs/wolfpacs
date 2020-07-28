-module(prop_presentation_context_accept).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(_, term(),
	    begin
		PrCID = 1,
		TransferSyntax = <<"1.2.840.10008.1.2">>,
		Encoded = wolfpacs_presentation_context_accept:encode(no_flow, PrCID, TransferSyntax),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.20),
		case wolfpacs_presentation_context_accept:decode(no_flow, Corrupt) of
		    {ok, _, _, _} ->
			true;
		    error ->
			true;
		    _  ->
			false
		end
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
