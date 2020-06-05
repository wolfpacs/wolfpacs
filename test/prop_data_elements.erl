-module(prop_data_elements).
-include_lib("proper/include/proper.hrl").
-import(wolfpacs_test_generators, [strategy/0,
				   valid_strategy/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_decode_test() ->
    ?FORALL({Blob, Strategy}, {binary(), strategy()},
	    begin
		{ok, Flow} = wolfpacs_flow:start_link(),
		case wolfpacs_data_elements:decode(Flow, Strategy, Blob) of
		    {ok, _, _} ->
			true;
		    error ->
			true;
		    _ ->
			false
		end
	    end).

-define(CMD, 16#0000).
-define(UID, 16#0002).
-define(FLD, 16#0100).

-define(RQID, 16#0110).
-define(RPID, 16#0120).
-define(SET, 16#0800).
-define(STU, 16#0900).

prop_fuzz_decode_test() ->
    ?FORALL(Strategy, valid_strategy(),
	    begin
		{ok, Flow} = wolfpacs_flow:start_link(),
		UID = <<"1.2.3.4">>,
		Items = #{{?CMD, ?UID} => UID,
			  {?CMD, ?FLD} => 16#8030,
			  {?CMD, ?RPID} => ?RQID,
			  {?CMD, ?SET} => 16#0101,
			  {?CMD, ?STU} => 16#0000},
		Encoded = wolfpacs_data_elements:encode(Flow, Strategy, Items),
		Corrupt = wolfpacs_utils:random_clear(Encoded, 0.2),
		case wolfpacs_data_elements:decode(Flow, Strategy, Corrupt) of
		    {ok, _Map, _Rest} ->
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
