-module(prop_vr_da).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_random_clear_test() ->
    ?FORALL(_, term(),
	    begin
 		Strategy = {explicit, little},
		Info = <<1, 2, 3, 4>>,
		Encoded = wolfpacs_vr_da:encode(no_flow, Strategy, Info),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_vr_da:decode(no_flow, Strategy, Corrupt) of
		    {ok, _, _} ->
			true;
		    error ->
			true;
		    _ ->
			false
		end
	    end).

prop_decode_test() ->
    ?FORALL(Data, binary(),
	    begin
 		Strategy = {explicit, little},
		case wolfpacs_vr_da:decode(no_flow, Strategy, Data) of
		    {ok, _, _} ->
			true;
		    error ->
			true;
		    _ ->
			false
		end
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
