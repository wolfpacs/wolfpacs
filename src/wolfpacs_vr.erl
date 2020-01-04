%%%-------------------------------------------------------------------
%% @doc Value Representation (VR.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr).
-export([ui/1, ae/1]).

-spec ae(binary()) -> binary().
ae(AE) ->
    exact(AE, 16).

ui(UID) ->
    limit(pad(UID), 64).

%%==============================================================================
%% Private
%%==============================================================================

pad(Item) ->
    case has_odd_length(Item) of
	true -> Item ++ " ";
	false -> Item
    end.

limit(Item, Length) when length(Item) > Length ->
    {Keep, _Drop} = lists:split(Length, Item),
    Keep;
limit(Item, _Length) ->
    Item.

has_odd_length(Item) ->
    (length(Item) band 1) == 1.

exact(Item, Length) ->
    NbBytes = byte_size(Item),
    exact(Item, Length, NbBytes).

exact(Item, Length, NbBytes) when Length == NbBytes ->
    Item;
exact(Item, Length, NbBytes) when Length > NbBytes ->
    Missing = Length - NbBytes,
    Padding = list_to_binary([" " || _ <- lists:seq(1, Missing)]),
    <<Item/binary, Padding/binary>>;
exact(Item, Length, _NbBytes) ->
    Bytes = 8 * Length,
    <<Correct:Bytes/bitstring, _/binary>> = Item,
    Correct.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

limit_test_() ->
    [ ?_assert(limit("ABC", 4) =:= "ABC"),
      ?_assert(limit("ABC", 3) =:= "ABC"),
      ?_assert(limit("ABC", 2) =:= "AB" )].

exact_test_() ->
    Item = <<"ABCD">>,
    [ ?_assertEqual(exact(Item, 4), <<"ABCD">>),
      ?_assertEqual(exact(Item, 2), <<"AB">>),
      ?_assertEqual(exact(Item, 6), <<"ABCD  ">>) ].
