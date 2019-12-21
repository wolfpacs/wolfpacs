-module(wolfpacs_utils).
-include_lib("eunit/include/eunit.hrl").
-export([drop_last_byte/1,
	 split/2]).

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
