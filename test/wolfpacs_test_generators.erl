-module(wolfpacs_test_generators).
-include_lib("proper/include/proper.hrl").
-export([strategy/0]).

strategy() ->
    Available = [ {explicit, little}
		, {explicit, big}
 		, {implicit, little}
		, {implicit, big}
		, badStrategy
		, {badType, badEndian}
		],
    oneof(Available).
