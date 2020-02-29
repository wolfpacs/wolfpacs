-module(prop_group_elements).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_vr_test() ->
    ?FORALL({Group, Element}, {integer(), integer()},
	    begin
		VR = wolfpacs_group_elements:vr(Group, Element),
		is_list(VR)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
