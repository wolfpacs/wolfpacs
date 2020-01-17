%%%-------------------------------------------------------------------
%% @doc Value Representation Application Entity.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ae).
-export([encode/1,
	 decode/1]).

-spec encode(list()) -> binary().
encode(UI) ->
    list_to_binary(limit(pad(UI))).

-spec decode(binary()) -> list().
decode(Data) ->
    string:strip(binary_to_list(Data)).

%%==============================================================================
%% Private
%%==============================================================================

has_odd_length(Item) ->
    (length(Item) band 1) == 1.

pad(Item) ->
    case has_odd_length(Item) of
	true -> Item ++ " ";
	false -> Item
    end.

limit(UI) when length(UI) > 16->
    {Keep, _Drop} = lists:split(16, UI),
    Keep;
limit(UI) ->
    UI.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [?_assertEqual(encode(""), <<"">>),
     ?_assertEqual(encode("A"), <<"A ">>),
     ?_assertEqual(encode("AB"), <<"AB">>),
     ?_assertEqual(encode("ABC"), <<"ABC ">>),
     ?_assertEqual(encode("ABCD"), <<"ABCD">>) ].

encode_decode_test_() ->
    Long = [$A || _ <- lists:seq(1, 32)],
    Trimmed = [$A || _ <- lists:seq(1, 16)],
    [?_assertEqual(decode(encode("")), ""),
     ?_assertEqual(decode(encode("A")), "A"),
     ?_assertEqual(decode(encode("AB")), "AB"),
     ?_assertEqual(decode(encode("ABC")), "ABC"),
     ?_assertEqual(decode(encode("ABCD")), "ABCD"),
     ?_assertEqual(decode(encode(Long)), Trimmed) ].
