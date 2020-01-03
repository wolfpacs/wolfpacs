-module(wolfpacs_item_inspector).
-include_lib("eunit/include/eunit.hrl").
-export([find/2]).

find(Data, ItemType) ->
    find(Data, ItemType, []).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

find(<<ItemType, 0, Length:16, Data/binary>>, ItemType, Acc) when Length >= 1 ->
    Size = byte_size(Data),
    case Length >= 1 andalso Length =< Size of
	true ->
	    Item = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, Size - Length),
	    find(Rest, ItemType, [Item|Acc]);
	false ->
	    find(<<Length:16, Data/binary>>, ItemType, Acc)
    end;
find(<<_, Next/binary>>, ItemType, Acc) ->
    find(Next, ItemType, Acc);
find(<<>>, _, Acc) ->
    lists:reverse(Acc).

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

find_test_() ->
    [
     ?_assert(find(<<>>, 1) =:= []),
     ?_assert(find(<<12, 0, 2:16, 42, 43>>, 1) =:= []),
     ?_assert(find(<<1, 0, 2:16, 42, 43>>, 1) =:= [<<42, 43>>]),
     ?_assert(find(<<12, 1, 0, 2:16, 42, 43>>, 1) =:= [<<42, 43>>]),
     ?_assert(find(<<12, 1, 0, 3:16, 42, 43>>, 1) =:= [])
    ].
