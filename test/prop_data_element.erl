-module(prop_data_element).
-include_lib("proper/include/proper.hrl").
-import(wolfpacs_test_generators, [strategy/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_decode_test() ->
    ?FORALL({Blob, Strategy}, {binary(), strategy()},
	    begin
		{ok, Flow} = wolfpacs_flow:start_link(),
		case wolfpacs_data_element:decode(Flow, Strategy, Blob) of
		    error -> true;
		    {ok, _, _} -> true;
		    _ -> false
		end
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
