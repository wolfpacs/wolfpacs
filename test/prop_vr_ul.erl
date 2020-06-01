-module(prop_vr_ul).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_random_clear_test() ->
    ?FORALL(UL, pos_integer(),
	    begin
 		Strategy = {explicit, little},
		Encoded = wolfpacs_vr_ul:encode(no_flow, Strategy, UL),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_vr_ul:decode(no_flow, Strategy, Corrupt) of
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
		case wolfpacs_vr_ul:decode(no_flow, Strategy, Data) of
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
