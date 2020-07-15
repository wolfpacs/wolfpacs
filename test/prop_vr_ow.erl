-module(prop_vr_ow).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_random_clear_test() ->
    ?FORALL(Info, list(byte()),
	    begin
 		Strategy = {explicit, little},
		Encoded = wolfpacs_vr_ow:encode(no_flow, Strategy, Info),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_vr_ow:decode(no_flow, Strategy, Corrupt) of
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
		Strategy = wolfpacs_test_generators:valid_strategy(),
		case wolfpacs_vr_ow:decode(no_flow, Strategy, Data) of
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
