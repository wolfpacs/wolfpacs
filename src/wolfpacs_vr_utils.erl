%%%-------------------------------------------------------------------
%% @doc Value Representation Utilities.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_utils).
-export([pad/1,
	 pad_binary/1,
	 limit/2,
	 limit_binary/2,
	 exact_binary/2,
	 trim/1,
	 trim_binary/1]).

pad(Item) ->
    case has_odd_length(Item) of
	true -> Item ++ " ";
	false -> Item
    end.

limit(Item, Max) when length(Item) > Max->
    {Keep, _Drop} = lists:split(Max, Item),
    Keep;
limit(Item, _) ->
    Item.

limit_binary(Item, Max) when byte_size(Item) > Max ->
    binary:part(Item, 0, Max);
limit_binary(Item, _) ->
    Item.

exact_binary(Item, Target) when is_list(Item) ->
    exact_binary(list_to_binary(Item), Target);
exact_binary(Item, Target) ->
    NbBytes = byte_size(Item),
    exact_binary(Item, Target, NbBytes).

exact_binary(Item, Target, Target) ->
    Item;
exact_binary(Item, Target, NbBytes) when NbBytes > Target ->
    binary:part(Item, 0, Target);
exact_binary(Item, Target, NbBytes) ->
    MissingBits = (Target - NbBytes) * 8,
    <<Item/binary, 0:MissingBits>>.

pad_binary(Item) ->
    case has_odd_length(Item) of
	true -> <<Item/binary, 0>>;
	false -> Item
    end.

trim(Item) when is_binary(Item) ->
    trim(binary_to_list(Item));
trim(Item) ->
    string:strip(string:strip(Item, right, 0),
		 right, 32).

trim_binary(Item) ->
    list_to_binary(trim(binary_to_list(Item))).

%%==============================================================================
%% Private
%%==============================================================================

has_odd_length(Item) when is_binary(Item) ->
    (byte_size(Item) band 1) == 1;

has_odd_length(Item) when is_list(Item) ->
    (length(Item) band 1) == 1.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

pad_test_() ->
    [?_assertEqual(pad("A"), "A "),
     ?_assertEqual(pad("AB"), "AB"),
     ?_assertEqual(pad("ABC"), "ABC "),
     ?_assertEqual(pad("ABCD"), "ABCD")].

limit_test_() ->
    [?_assertEqual(limit("ABC", 1), "A"),
     ?_assertEqual(limit("ABC", 2), "AB"),
     ?_assertEqual(limit("ABC", 3), "ABC"),
     ?_assertEqual(limit("ABC", 4), "ABC") ].

exact_binary_test_() ->
    [?_assertEqual(exact_binary(<<"AB">>, 3), <<"AB", 0>>),
     ?_assertEqual(exact_binary(<<"AB">>, 2), <<"AB">>),
     ?_assertEqual(exact_binary(<<"AB">>, 1), <<"A">>) ].

trim_test_() ->
    [?_assertEqual(trim(" "), ""),
     ?_assertEqual(trim("A "), "A"),
     ?_assertEqual(trim("AB"), "AB"),
     ?_assertEqual(trim("ABC "), "ABC"),
     ?_assertEqual(trim("ABCD"), "ABCD") ].
