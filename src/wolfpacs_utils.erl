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
	 clear_byte/2]).

-spec drop_first_byte(binary()) -> binary().
drop_first_byte(<<_, Data/binary>>) ->
    Data.

-spec drop_last_byte(binary()) -> binary().
drop_last_byte(Data) ->
    list_to_binary(lists:droplast(binary_to_list(Data))).

-spec split(binary(), non_neg_integer()) -> {ok, binary(), binary()} | {error, binary()}.
split(Data, At) when At >= 0 ->
    Size = byte_size(Data),
    case At =< Size of
	true ->
	    L = binary:part(Data, 0, At),
	    R = binary:part(Data, At, Size - At),
	    {ok, L, R};
	false ->
	    {error, Data}
    end;
split(Data, _) ->
    {error, Data}.

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

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

drop_first_byte_test_() ->
    [?_assert(drop_first_byte(<<1, 2, 3>>) =:= <<2 ,3>>) ].

drop_last_byte_test_() ->
    [?_assert(drop_last_byte(<<1, 2, 3>>) =:= <<1 ,2>>) ].

split_test_() ->
    [
     ?_assert(split(<<1, 2, 3, 4>>, 4) =:= {ok, <<1 ,2, 3 ,4>>, <<>>}),
     ?_assert(split(<<1, 2, 3, 4>>, 3) =:= {ok, <<1 ,2, 3>>, << 4>>}),
     ?_assert(split(<<1, 2, 3, 4>>, 2) =:= {ok, <<1 ,2>>, <<3, 4>>}),
     ?_assert(split(<<1, 2, 3, 4>>, 1) =:= {ok, <<1>>, <<2, 3, 4>>}),
     ?_assert(split(<<1, 2, 3, 4>>, 5) =:= {error, <<1, 2, 3, 4>>})
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
