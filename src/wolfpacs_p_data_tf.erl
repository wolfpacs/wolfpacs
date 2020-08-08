%%%-------------------------------------------------------------------
%% @doc P Data TF
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_p_data_tf).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), list(#pdv_item{})) -> binary().
encode(Flow, PDVItems) ->
    Data = priv_encode(Flow, PDVItems, []),
    Length = byte_size(Data),
    <<16#4, 0, Length:32, Data/binary>>.

-spec decode(flow(), binary()) -> {ok, list(#pdv_item{}), binary()} | error.
decode(Flow, <<16#4, _, Length:32, Data/binary>>) ->
    case wolfpacs_utils:split(Data, Length) of
	{ok, PDVItemsData, Rest} ->
	    case decode_items(Flow, PDVItemsData, []) of
		{ok, PDVItems} ->
		    {ok, PDVItems, Rest};
		_ ->
		    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode items"),
		    error
		end;
	_ ->
	    wolfpacs_flow:warning(Flow, ?MODULE, "unable to split"),
	    error
    end;
decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

%%============================================================================
%% Private
%%============================================================================

priv_encode(_Flow, [], Acc) ->
    encode_payload(lists:reverse(Acc), <<>>);
priv_encode(Flow, [PDVItem|PDVItems], Acc) ->
    Data = wolfpacs_pdv_item:encode(Flow, PDVItem),
    priv_encode(Flow, PDVItems, [Data|Acc]).

encode_payload([], Acc) ->
    Acc;
encode_payload([H|T], Acc) ->
    encode_payload(T, <<Acc/binary, H/binary>>).

decode_items(_Flow, <<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_items(Flow, Data, Acc) ->
    case wolfpacs_pdv_item:decode(Flow, Data) of
	{ok, PDVItem, Rest} ->
	    decode_items(Flow, Rest, [PDVItem|Acc]);
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
    Encoded = encode(no_flow, PDVItems),
    LastByte = lists:last(binary_to_list(Encoded)),
    ?assertEqual(LastByte, ?LAST_BYTE).

encode_decode_test_() ->
    PDVItems = test_items(),

    Encoded0 = encode(no_flow, PDVItems),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = wolfpacs_utils:clear_even(Encoded0),
    Incorrect3 = <<12, 13, 14, 15>>,
    Incorrect4 = <<>>,

    [ ?_assertEqual(decode(no_flow, Encoded0), {ok, PDVItems, <<>>})
    , ?_assertEqual(decode(no_flow, Encoded1), {ok, PDVItems, <<42>>})
    , ?_assertEqual(decode(no_flow, Incorrect0), error)
    , ?_assertEqual(decode(no_flow, Incorrect1), error)
    , ?_assertEqual(decode(no_flow, Incorrect2), error)
    , ?_assertEqual(decode(no_flow, Incorrect3), error)
    , ?_assertEqual(decode(no_flow, Incorrect4), error)
    ].

decode_pdu_4_test_() ->
    HexlData =
	"0400 0000 0114 0000 0110 0102 0800 0500
         4353 0a00 4953 4f5f 4952 2031 3030 0800
         1600 5549 1c00 312e 322e 3834 302e 3130
         3030 382e 352e 312e 342e 312e 312e 3131
         2e31 0800 1800 5549 3800 312e 322e 3237
         362e 302e 3732 3330 3031 302e 332e 312e
         342e 3833 3233 3332 392e 3132 3430 332e
         3135 3839 3831 3134 3933 2e32 3039 3434
         3400 0800 5000 5348 0000 0800 6000 4353
         0200 5052 1800 2200 4353 0200 4653 2000
         1000 5348 0000 2000 1100 4953 0400 3530
         3720 2000 1300 4953 0200 3820 2800 5210
         4453 0200 3020 2800 5410 4c4f 0200 5553
         7000 6000 5351 0000 3e00 0000 feff 00e0
         3600 0000 7000 0200 4353 1000 414e 4e4f
         5441 5449 4f4e 204c 4159 4552 7000 6200
         4953 0200 3120 7000 6800 4c4f 0c00 416e
         6e6f 7461 7469 6f6e 7320",
    Encoded = wolfpacs_utils:hexl_log_to_binary(HexlData),
    282 = byte_size(Encoded),
    {ok, [PDVItem], <<>>} = decode(no_flow, Encoded),
    #pdv_item{pr_cid = 1, is_last = true, is_command = false, pdv_data = PDVData} = PDVItem,
    {ok, DataSet, <<>>} = wolfpacs_data_elements:decode(no_flow, {explicit, little}, PDVData),
    Get = fun(H) -> maps:get(H, DataSet) end,
    SOPInstanceUID = <<"1.2.276.0.7230010.3.1.4.8323329.12403.1589811493.209444">>,

    [ ?_assertEqual(Get({16#8, 16#18}), SOPInstanceUID) ].
