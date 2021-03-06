-module(prop_vr_pn).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_random_clear_test() ->
    ?FORALL(Info, binary(),
	    begin
 		Strategy = {explicit, little},
		Encoded = wolfpacs_vr_pn:encode(no_flow, Strategy, Info),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_vr_pn:decode(no_flow, Strategy, Corrupt) of
		    {ok, _, _} ->
			true;
		    {error, Corrupt, _} ->
			true;
		    _ ->
			false
		end
	    end).

prop_decode_test() ->
    ?FORALL(Data, binary(),
	    begin
 		Strategy = {explicit, little},
		case wolfpacs_vr_pn:decode(no_flow, Strategy, Data) of
		    {ok, _, _} ->
			true;
		    {error, Data, _} ->
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
