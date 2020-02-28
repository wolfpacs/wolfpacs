-module(prop_file_meta_information).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(_, term(),
	    begin
		Strategy = {explicit, little},
		Info = #{{2, 1} => [0, 1],
			 {2, 2} => <<"1.2.840.10008.5.1.4.1.1.2">>},
		Encoded = wolfpacs_file_meta_information:encode(Strategy, Info),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_file_meta_information:decode({explicit, little}, Corrupt) of
		    {ok, _Map, _Rest} ->
			true;
		    {error, _Corrupt, _Error} ->
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
