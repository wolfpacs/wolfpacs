-module(prop_data_elements).
-include_lib("proper/include/proper.hrl").
-import(wolfpacs_test_generators, [strategy/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_decode_test() ->
    ?FORALL({Blob, Strategy}, {binary(), strategy()},
	    begin
		case wolfpacs_data_elements:decode(Strategy, Blob) of
		    {error, Blob, _} -> true;
		    _ -> false
		end
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
