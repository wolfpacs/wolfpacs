%%%-------------------------------------------------------------------
%% @doc DIMSE Protocol.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_dimse_protocol).
-export([encode/1,
	 attach_header/1,
	 decode/1]).

-spec encode(list({integer(), integer(), atom(), any()})) -> binary().
encode(Items) ->
    encode(Items, []).

attach_header(Data) ->
    NbBytes = byte_size(Data),
    Items = [{0, 0, ul, NbBytes}],
    Header = encode(Items),
    <<Header/binary, Data/binary>>.

decode(Data) ->
    case split_items(Data, []) of
	{ok, Items} ->
	    decode_items(Items, []);
	_ ->
	    {error, Data}
    end.

%%==============================================================================
%% Private
%%==============================================================================

pad_binary(Item) ->
    case (byte_size(Item) band 1) of
	1 -> <<Item/binary, " ">>;
	0 -> Item
    end.

pad_length(Length) ->
    case (Length band 1) of
	1 -> Length + 1;
	0 -> Length
    end.

-spec encode(list({integer(), integer(), atom(), any()}), list(binary())) -> binary().
encode([], Acc) ->
    combine(lists:reverse(Acc), <<>>);
encode([{Group, Element, us, Value}|Items], Acc) ->
    Data = <<Group:16/little, Element:16/little,
	     2:32/little, Value:16/little>>,
    encode(Items, [Data|Acc]);
encode([{Group, Element, ul, Value}|Items], Acc) ->
    Data = <<Group:16/little, Element:16/little,
	     4:32/little, Value:32/little>>,
    encode(Items, [Data|Acc]);
encode([{Group, Element, ui, Value}|Items], Acc) ->
    PaddedLength = pad_length(byte_size(Value)),
    PaddedValue = pad_binary(Value),
    Data = <<Group:16/little, Element:16/little,
	     PaddedLength:32/little, PaddedValue/binary>>,
    encode(Items, [Data|Acc]);
encode([Item|Items], Acc) ->
    lager:warning("[dimse_protocol] unable to encode ~p", [Item]),
    encode(Items, Acc).

combine([], Acc) ->
    Acc;
combine([H|T], Acc) ->
    combine(T, <<Acc/binary, H/binary>>).

split_items(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
split_items(<<G:16/little, E:16/little, Length:32/little, Data/binary>>, Acc) ->
    case wolfpacs_utils:split(Data, Length) of
 	{ok, Part, Rest} ->
	    split_items(Rest, [{G, E, Part}|Acc]);
	_ ->
	    error
    end.

decode_items([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_items([{G, E, Data}|T], Acc) ->
    Res = case wolfpacs_implicit:vr(G, E) of
	      us ->
		  <<V:16/little>> = Data,
		  {G, E, us, V};
	      ul ->
		  <<V:32/little>> = Data,
		  {G, E, ul, V};
	      ui ->
		  {G, E, ui, Data};
	      VR ->
		  {G, E, VR, Data}
	  end,
    decode_items(T, [Res|Acc]).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    G0 = 16#0000,
    E0 = 16#0000,
    E1 = 16#0100,

    Item0 = {G0, E0, ul, 2},
    Item1 = {G0, E1, us, 3},

    Data0 = <<G0:16/little, E0:16/little, 4:32/little, 2:32/little>>,
    Data1 = <<G0:16/little, E1:16/little, 2:32/little, 3:16/little>>,

    [?_assertEqual(encode([Item0]), Data0),
     ?_assertEqual(encode([Item1]), Data1),
     ?_assertEqual(encode([Item0, Item1]), <<Data0/binary, Data1/binary>>)
    ].

encode_decode_test_() ->
    G0 = 16#0000,
    E0 = 16#0000,
    E1 = 16#0100,

    Item0 = {G0, E0, ul, 2},
    Item1 = {G0, E1, us, 3},

    [?_assertEqual(decode(encode([Item0])), {ok, [Item0]}),
     ?_assertEqual(decode(encode([Item1])), {ok, [Item1]}),
     ?_assertEqual(decode(encode([Item0, Item1])), {ok, [Item0, Item1]})
    ].
