-module(wolfpacs_utils).
-include_lib("eunit/include/eunit.hrl").
-export([drop_last_byte/1,
	 split/2,
	 log_hex_to_int/1,
	 log_to_binary/1]).

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
    Tokens = string:tokens(LogData, ", \n\t"),
    list_to_binary(lists:map(fun log_hex_to_int/1, Tokens)).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

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

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

drop_last_byte_test_() ->
    [?_assert(drop_last_byte(<<1, 2, 3>>) =:= <<1 ,2>>) ].

split_test_() ->
    [
     ?_assert(split(<<1, 2, 3, 4>>, 4) =:= {ok, <<1 ,2, 3 ,4>>, <<>>}),
     ?_assert(split(<<1, 2, 3, 4>>, 3) =:= {ok, <<1 ,2, 3>>, << 4>>}),
     ?_assert(split(<<1, 2, 3, 4>>, 2) =:= {ok, <<1 ,2>>, <<3, 4>>}),
     ?_assert(split(<<1, 2, 3, 4>>, 1) =:= {ok, <<1>>, <<2, 3, 4>>})
    ].

log_to_binary_test_() ->
    Value0 = "31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 31, 2e, 32",
    Value1 = "31 2e 32 2e 38 34 30 2e 31 30 30 30 38 2e 31 2e 32",
    Correct0 = <<"1.2.840.10008.1.2">>,
    Correct1 = Correct0,
    [ ?_assertEqual(log_to_binary(Value0), Correct0),
      ?_assertEqual(log_to_binary(Value1), Correct1) ].
