-module(wolfpacs_binary_distance).
-export([binary_distance/2]).

binary_distance(<<>>, YS) ->
    byte_size(YS);
binary_distance(XS, <<>>) ->
    byte_size(XS);
binary_distance(XS, YS) ->
    AS = binary_to_indexed_list(XS),
    BS = binary_to_indexed_list(YS),
    Matrix = [{A, B} ||
		 A <- AS,
		 B <- BS],
    Map = populate(byte_size(XS), byte_size(YS)),
    priv_binary_distance(Matrix, Map).

%%==============================================================================
%% Private
%%==============================================================================

binary_to_indexed_list(Data) ->
    Chars = binary_to_list(Data),
    IS = lists:seq(1, length(Chars)),
    lists:zip(IS, Chars).

populate(M, N) ->
    Map1 = maps:from_list([{{I, 0}, I} || I <- lists:seq(0, M-1)]),
    Map2 = maps:from_list([{{0, J}, J} || J <- lists:seq(0, N-1)]),
    maps:merge(Map1, Map2).

best_option(X, Y, Z) ->
    min(min(X, Y), Z).

priv_binary_distance([], Map) ->
    Map;
priv_binary_distance([{{I, X}, {J, X}}|Rest], Map) ->
    Best = best_option(maps:get({I-1, J}, Map) + 1,
		       maps:get({I, J-1}, Map) + 1,
		       maps:get({I-1, J-1}, Map) + 0),
    lager:warning("[~p, ~p]: ~p", [I, J, Best]),
    priv_binary_distance(Rest, Map#{{I, J} => Best});
priv_binary_distance([{{I, _}, {J, _}}|Rest], Map) ->
    Best = best_option(maps:get({I-1, J}, Map) + 1,
		       maps:get({I, J-1}, Map) + 1,
		       maps:get({I-1, J-1}, Map) + 1),
    lager:warning("[~p, ~p]: ~p", [I, J, Best]),
    priv_binary_distance(Rest, Map#{{I, J} => Best}).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

populate_test() ->
    ?assertEqual(populate(3, 2),
		 #{{0, 0} => 0,
		   {1, 0} => 1,
		   {2, 0} => 2,
		   {0, 1} => 1}).

binary_to_indexed_list_test() ->
    ?assertEqual(binary_to_indexed_list(<<"ABCD">>),
		 [{1, $A}, {2, $B}, {3, $C}, {4, $D}]).

%% binary_to_indexed_list_test_() ->
%%     [ ?_assertEqual(binary_to_indexed_list(<<20, 30, 40>>),
%% 		    [{0, 20}, {1, 30}, {2, 40}])
%%     ].
%%
%% binary_distance_base_cases_test_() ->
%%     [ ?_assertEqual(binary_distance(<<>>, <<>>), 0)
%%     , ?_assertEqual(binary_distance(<<"cat">>, <<>>), 3)
%%     , ?_assertEqual(binary_distance(<<>>, <<"dog">>), 3)
%%     , ?_assertEqual(binary_distance(<<"a">>, <<"aaa">>), 2)
%%     , ?_assertEqual(binary_distance(<<"foo">>, <<"foo">>), 0)
%%     ].
%%
