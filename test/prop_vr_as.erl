-module(prop_vr_as).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_random_clear_test() ->
    ?FORALL(Info, binary(),
	    begin
		{ok, Flow} = wolfpacs_flow:start_link(),
		Strategy = {explicit, little},
		Encoded = wolfpacs_vr_as:encode(Flow, Strategy, Info),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_vr_as:decode(Flow, Strategy, Corrupt) of
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
		{ok, Flow} = wolfpacs_flow:start_link(),
		Strategy = wolfpacs_test_generators:valid_strategy(),
		case wolfpacs_vr_as:decode(Flow, Strategy, Data) of
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
