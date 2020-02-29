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
		Encoded = wolfpacs_presentation_context_accept:encode(PrCID, TransferSyntax),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.20),
		case wolfpacs_presentation_context_accept:decode(Corrupt) of
		    {ok, _, _, _} ->
			true;
		    {error, Corrupt, _Error} ->
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
