%%%-------------------------------------------------------------------
%% @doc P Data TF
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_p_data_tf).
-export([encode/1,
	 decode/1]).
-include("wolfpacs_types.hrl").

-spec encode(list(#pdv_item{})) -> binary().
encode(PDVItems) ->
    Data = encode(PDVItems, []),
    Length = byte_size(Data),
    <<16#4, 0, Length:32, Data/binary>>.

decode(AllData = <<16#4, _, Length:32, Data/binary>>) ->
    case wolfpacs_utils:split(Data, Length) of
	{ok, PDVItemsData, Rest} ->
	    case decode_items(PDVItemsData, []) of
		{ok, PDVItems} ->
		    {ok, PDVItems, Rest};
		_ ->
		    {error, AllData}
		end;
	_ ->
	    {error, AllData}
    end;
decode(AllData) ->
    {error, AllData}.

%%============================================================================
%% Private
%%============================================================================

encode([], Acc) ->
    encode_payload(lists:reverse(Acc), <<>>);
encode([PDVItem|PDVItems], Acc) ->
    Data = wolfpacs_pdv_item:encode(PDVItem),
    encode(PDVItems, [Data|Acc]).

encode_payload([], Acc) ->
    Acc;
encode_payload([H|T], Acc) ->
    encode_payload(T, <<Acc/binary, H/binary>>).

decode_items(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_items(Data, Acc) ->
    case wolfpacs_pdv_item:decode(Data) of
	{ok, PDVItem, Rest} ->
	    decode_items(Rest, [PDVItem|Acc]);
	_ ->
	    error
    end.

%%============================================================================
%% Test
%%============================================================================

-include_lib("eunit/include/eunit.hrl").
-define(LAST_BYTE, 72).

test_items() ->
    PDVData0 = <<1, 2, 3, 4, 5>>,
    PDVItem0 = #pdv_item{pr_cid=42,
			 is_command=true,
			 is_last=false,
			 pdv_data=PDVData0},
    PDVData1 = <<32, 12, 43, ?LAST_BYTE>>,
    PDVItem1 = #pdv_item{pr_cid=43,
			 is_command=false,
			 is_last=false,
			 pdv_data=PDVData1},
    [PDVItem0, PDVItem1].

encode_test() ->
    PDVItems = test_items(),
    Encoded = encode(PDVItems),
    LastByte = lists:last(binary_to_list(Encoded)),
    ?assertEqual(LastByte, ?LAST_BYTE).

encode_decode_test_() ->
    PDVItems = test_items(),

    Encoded0 = encode(PDVItems),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = <<12, 13, 14, 15>>,

    [?_assertEqual(decode(Encoded0), {ok, PDVItems, <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, PDVItems, <<42>>}),
     ?_assertEqual(decode(Incorrect0), {error, Incorrect0})].
