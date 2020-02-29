-module(prop_p_data_tf).
-include_lib("proper/include/proper.hrl").
-include("wolfpacs_types.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(_, term(),
	    begin
		PDVItems = test_items(),
		Encoded = wolfpacs_p_data_tf:encode(PDVItems),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_p_data_tf:decode(Corrupt) of
		    {ok, _Items, _Rest} ->
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
-define(LAST_BYTE, 72).

test_items() ->
    PDVData0 = <<1, 2, 3, 4, 5>>,
    PDVItem0 = #pdv_item{pr_cid=42,
			 is_command=true,
			 is_last=false,
			 pdv_data=PDVData0},
    PDVData1 = <<32, 12, 43, ?LAST_BYTE>>,
    PDVItem1 = #pdv_item{pr_cid=43,
			 is_command=false,
			 is_last=false,
			 pdv_data=PDVData1},
    [PDVItem0, PDVItem1].

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
