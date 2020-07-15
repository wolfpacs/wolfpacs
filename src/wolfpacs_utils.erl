%%%-------------------------------------------------------------------
%% @doc Utils.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_utils).
-export([drop_last_byte/1,
	 drop_first_byte/1,
	 split/2,
	 log_hex_to_int/1,
	 log_to_binary/1,
	 hexl_log_to_binary/1,
	 clear_byte/2,
	 remove_keys/2,
	 clear_even/1,
	 clear_odd/1,
	 random_clear/2,
	 chunk/2]).

-spec drop_first_byte(binary()) -> binary().
drop_first_byte(<<_, Data/binary>>) ->
    Data.

-spec drop_last_byte(binary()) -> binary().
drop_last_byte(Data) ->
    list_to_binary(lists:droplast(binary_to_list(Data))).

-spec split(binary(), non_neg_integer()) -> {ok, binary(), binary()} | {error, binary(), list(string())}.
split(Data, At) when At >= 0 ->
    Size = byte_size(Data),
    case At =< Size of
	true ->
	    L = binary:part(Data, 0, At),
	    R = binary:part(Data, At, Size - At),
	    {ok, L, R};
	false ->
	    {error, Data, ["not enough data to split"]}
    end;
split(Data, _) ->
    {error, Data, ["incorrect header"]}.

log_hex_to_int([L]) ->
    hv(L);
log_hex_to_int([H, L]) ->
    hv(H) * 16 + hv(L).

log_to_binary(LogData) ->
    Cleaned = re:replace(LogData, "D:", "", [global, {return, list}]),
    Tokens = string:tokens(Cleaned, ", \n\t"),
    list_to_binary(lists:map(fun log_hex_to_int/1, Tokens)).

clear_byte(Data, At) ->
    Size = byte_size(Data),
    case At < Size of
	true ->
	    Head = binary:part(Data, 0, At),
	    Tail = binary:part(Data, At+1, Size - At - 1),
	    {ok, <<Head/binary, 0, Tail/binary>>};
	false ->
	    {error, Data}
    end.

remove_keys([], Map) ->
    Map;
remove_keys([Key|Keys], Map) ->
    remove_keys(Keys, maps:remove(Key, Map)).

clear_even(Data) ->
    clear_data(binary_to_list(Data), [], now).

clear_odd(Data) ->
    clear_data(binary_to_list(Data), [], next).

random_clear(Data, FlipPropability) ->
    random_clear(Data, FlipPropability, []).

hexl_log_to_binary(Data) ->
    Tokens = string:tokens(Data, ",: \n\t"),
    list_to_binary(lists:map(fun log_hex_to_int/1, regroup_by_two(Tokens))).

chunk(Data, Size) ->
    chunk(Data, Size, []).

chunk(Data, Size, Acc) ->
    case split(Data, Size) of
	{ok, Chunk, <<>>} ->
	    lists:reverse([Chunk|Acc]);
	{ok, Chunk, Rest} ->
	    chunk(Rest, Size, [Chunk|Acc]);
	_ ->
	    lists:reverse([Data|Acc])
    end.

%%==============================================================================
%% Private
%%==============================================================================

hv($0) -> 0;
hv($1) -> 1;
hv($2) -> 2;
hv($3) -> 3;
hv($4) -> 4;
hv($5) -> 5;
hv($6) -> 6;
hv($7) -> 7;
hv($8) -> 8;
hv($9) -> 9;
hv($A) -> 10;
hv($B) -> 11;
hv($C) -> 12;
hv($D) -> 13;
hv($E) -> 14;
hv($F) -> 15;
hv($a) -> 10;
hv($b) -> 11;
hv($c) -> 12;
hv($d) -> 13;
hv($e) -> 14;
hv($f) -> 15.

clear_data([], Acc, _) ->
    list_to_binary(lists:reverse(Acc));
clear_data([_|T], Acc, now)->
    clear_data(T, [0|Acc], next);
clear_data([H|T], Acc, next) ->
    clear_data(T, [H|Acc], now).

random_clear(<<>>, _, Acc) ->
    list_to_binary(lists:reverse(Acc));
random_clear(<<Head, Tail/binary>>, FlipPropability, Acc) ->
    Flip = rand:uniform(),
    case Flip < FlipPropability of
	true ->
	    Byte = rand:uniform(256) - 1,
	    random_clear(Tail, FlipPropability, [Byte|Acc]);
	false  ->
	    random_clear(Tail, FlipPropability, [Head|Acc])
    end.

regroup_by_two(List) ->
    regroup_by_two(List, []).

regroup_by_two([], Acc) ->
    lists:reverse(Acc);
regroup_by_two([[A, B, C, D]|Rest], Acc) ->
    regroup_by_two(Rest, [[C, D], [A, B]|Acc]).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

drop_first_byte_test_() ->
    [?_assertEqual(drop_first_byte(<<1, 2, 3>>), <<2 ,3>>) ].

drop_last_byte_test_() ->
    [?_assertEqual(drop_last_byte(<<1, 2, 3>>), <<1 ,2>>) ].

split_test_() ->
    [
     ?_assertEqual(split(<<1, 2, 3, 4>>, 4), {ok, <<1 ,2, 3 ,4>>, <<>>}),
     ?_assertEqual(split(<<1, 2, 3, 4>>, 3), {ok, <<1 ,2, 3>>, << 4>>}),
     ?_assertEqual(split(<<1, 2, 3, 4>>, 2), {ok, <<1 ,2>>, <<3, 4>>}),
     ?_assertEqual(split(<<1, 2, 3, 4>>, 1), {ok, <<1>>, <<2, 3, 4>>}),
     ?_assertEqual(split(<<1, 2, 3, 4>>, 5), {error, <<1, 2, 3, 4>>, ["not enough data to split"]}),
     ?_assertEqual(split(<<1, 2, 3, 4>>, -1), {error, <<1, 2, 3, 4>>, ["incorrect header"]})
    ].

log_to_binary_test_() ->
    Value0 = "31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 31, 2e, 32",
    Value1 = "31 2e 32 2e 38 34 30 2e 31 30 30 30 38 2e 31 2e 32",
    Value2 = "1 2 3 4 5 6 7 8 9 A B C D E F",
    Correct0 = <<"1.2.840.10008.1.2">>,
    Correct1 = Correct0,
    Correct2 = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    [ ?_assertEqual(log_to_binary(Value0), Correct0),
      ?_assertEqual(log_to_binary(Value1), Correct1),
      ?_assertEqual(log_to_binary(Value2), Correct2) ].

clear_byte_test_() ->
    Value = <<0, 1, 2, 3, 4>>,
    [ ?_assertEqual(clear_byte(Value, 0), {ok, <<0, 1, 2, 3, 4>>}),
      ?_assertEqual(clear_byte(Value, 1), {ok, <<0, 0, 2, 3, 4>>}),
      ?_assertEqual(clear_byte(Value, 2), {ok, <<0, 1, 0, 3, 4>>}),
      ?_assertEqual(clear_byte(Value, 3), {ok, <<0, 1, 2, 0, 4>>}),
      ?_assertEqual(clear_byte(Value, 4), {ok, <<0, 1, 2, 3, 0>>}),
      ?_assertEqual(clear_byte(Value, 5), {error, <<0, 1, 2, 3, 4>>}) ].

remove_keys_test_() ->
    Map = #{1 => 2, 3 => 4, 5 => 6},
    [ ?_assertEqual(remove_keys([], Map), Map),
      ?_assertEqual(remove_keys([7], Map), Map),
      ?_assertEqual(remove_keys([1, 3, 5], Map), #{}),
      ?_assertEqual(remove_keys([1], Map), #{3 => 4, 5 => 6}),
      ?_assertEqual(remove_keys([1, 5], Map), #{3 => 4}) ].

clear_even_test_() ->
    [ ?_assertEqual(clear_even(<<>>), <<>>)
    , ?_assertEqual(clear_even(<<1>>), <<0>>)
    , ?_assertEqual(clear_even(<<1 ,2>>), <<0, 2>>)
    , ?_assertEqual(clear_even(<<1 ,2, 3>>), <<0, 2, 0>>)
    , ?_assertEqual(clear_even(<<1 ,2, 3, 4>>), <<0, 2, 0, 4>>)
    ].

clear_odd_test_() ->
    [ ?_assertEqual(clear_odd(<<>>), <<>>)
    , ?_assertEqual(clear_odd(<<1>>), <<1>>)
    , ?_assertEqual(clear_odd(<<1 ,2>>), <<1, 0>>)
    , ?_assertEqual(clear_odd(<<1 ,2, 3>>), <<1, 0, 3>>)
    , ?_assertEqual(clear_odd(<<1 ,2, 3, 4>>), <<1, 0, 3, 0>>)
    ].

random_clear_no_flip_test() ->
    Data = <<1, 2, 3, 4, 5, 6, 7, 8,
	     1, 2, 3, 4, 5, 6, 7, 8,
	     1, 2, 3, 4, 5, 6, 7, 8,
	     1, 2, 3, 4, 5, 6, 7, 8>>,
    ?assertEqual(random_clear(Data, 0.0), Data).

random_clear_flip_test() ->
    Data = <<0:64>>,
    ?assertNotEqual(random_clear(Data, 1.0), Data).

regroup_by_two_test_() ->
    [?_assertEqual(regroup_by_two(["1234", "4567"]), ["12", "34", "45", "67"])].

chunk_test_() ->
    [?_assertEqual(chunk(<<1, 2, 3, 4, 5>>, 2), [<<1, 2>>, <<3, 4>>, <<5>>])
    ].
