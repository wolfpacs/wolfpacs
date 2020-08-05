
%%%-------------------------------------------------------------------
%% @doc Value Representation Sequence Common.
%%
%% Design decision. Wolfpacs will always write out length explicitly.
%% However, we will support reading items with undefined length.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_sq_items).
-export([encode/3,
	 decode/3]).

%%-------------------------------------------------------------------
%% @doc Encode a SQ item.
%%
%% @end
%%-------------------------------------------------------------------

encode(Flow, Strategy, Items) ->
    wolfpacs_flow:start_encode(Flow, ?MODULE),
    encode(Flow, Strategy, Items, <<>>).

%%-------------------------------------------------------------------
%% @doc Decode a SQ item.
%%
%% @end
%%-------------------------------------------------------------------

decode(Flow, Strategy, Data) ->
    wolfpacs_flow:start_decode(Flow, ?MODULE),
    decode_item(Flow, Strategy, [], Data).

%%==============================================================================
%% Private
%%==============================================================================

encode(Flow, _Strategy, [], Encoded) ->
    wolfpacs_flow:success(Flow, ?MODULE),
    Encoded;

encode(Flow, Strategy, [Elements|Rest], Acc) ->
    Part = wolfpacs_vr_sq_item:encode(Flow, Strategy, Elements),
    encode(Flow, Strategy, Rest, <<Acc/binary, Part/binary>>).

%%-------------------------------------------------------------------
%% @doc Decode item
%%
%% @end
%%-------------------------------------------------------------------

decode_item(Flow, Strategy, Items, Data) ->
    NewItem = new_item(Strategy),
    EndSeq = end_seq(Strategy),
    case wolfpacs_vr_sq_item:decode(Flow, Strategy, Data) of
	{ok, Item, Rest = <<NewItem:32/bitstring, _/binary>>} ->
	    decode_item(Flow, Strategy, [Item|Items], Rest);
	{ok, Item, <<EndSeq:64/bitstring, Rest/binary>>} ->
	    {ok, [Item|Items], Rest};
	{ok, Item, Rest} ->
	    {ok, [Item|Items], Rest};
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode item"),
	    wolfpacs_flow:expected_32(Flow, ?MODULE, Data),
	    wolfpacs_flow:failed(Flow, ?MODULE, Data),
	    error
    end.

%%-------------------------------------------------------------------
%% @doc New Item in the data
%%
%% @end
%%-------------------------------------------------------------------

new_item({_, little}) ->
    <<16#fffe:16/little, 16#e000:16/little>>;
new_item({_, big}) ->
    <<16#fffe:16/big, 16#e000:16/big>>.

%%-------------------------------------------------------------------
%% @doc End sequence
%%
%% @end
%%-------------------------------------------------------------------
end_seq({_, little}) ->
    <<16#fffe:16/little, 16#e0dd:16/little, 0:32>>;
end_seq({_, big}) ->
    <<16#fffe:16/big, 16#e0dd:16/big, 0:32>>.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_little_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),

    Strategy = {implicit, little},
    Items = [#{{16#0008, 16#0100} => <<"121327">>,
	       {16#0008, 16#0102} => <<"DCM">>,
	       {16#0008, 16#0104} => <<"Full fidelity image">>
	      },
	     #{{16#0008, 16#0100} => <<"121327">>,
	       {16#0008, 16#0102} => <<"DCM">>,
	       {16#0008, 16#0104} => <<"Full fidelity image">>
	      }
	    ],

    Encoded0 = encode(Flow, Strategy, Items),
    Encoded1 = <<Encoded0/binary, 42>>,
    Encoded2 = wolfpacs_utils:drop_last_byte(Encoded0),
    Encoded3 = wolfpacs_utils:drop_first_byte(Encoded0),
    Encoded4 = <<1, 2, 3, 4, 5>>,

    Decoded0 = decode(Flow, Strategy, Encoded0),
    Decoded1 = decode(Flow, Strategy, Encoded1),
    Decoded2 = decode(Flow, Strategy, Encoded2),
    Decoded3 = decode(Flow, Strategy, Encoded3),
    Decoded4 = decode(Flow, Strategy, Encoded4),

    [ ?_assertEqual(Decoded0, {ok, Items, <<>>})
    , ?_assertEqual(Decoded1, {ok, Items, <<42>>})
    , ?_assertEqual(Decoded2, error)
    , ?_assertEqual(Decoded3, error)
    , ?_assertEqual(Decoded4, error)
    ].

nested_sq_explicit_little_test_() ->
    SOPClassUID = <<"1.2.840.10008.5.1.4.1.1.4">>,
    SOPInstance = <<"1.2.826.0.1.3680043.2.1125.1.92123297551800019460041377923358764">>,
    SeriesUID = <<"1.2.826.0.1.3680043.2.1125.1.73374395458721436255565291253139444">>,

    Correct = [#{{8,4416} => [#{{8,4432} => SOPClassUID,
				 {8,4437} => SOPInstance}],
		  {32,14} => SeriesUID
		 }],

    Encoded = wolfpacs_utils:hexl_log_to_binary(
		"" ++
		    "                         feff 00e0 ffff " ++
		    "ffff 0800 4011 5351 0000 ffff ffff feff " ++
		    "00e0 ffff ffff 0800 5011 5549 1a00 312e " ++
		    "322e 3834 302e 3130 3030 382e 352e 312e " ++
		    "342e 312e 312e 3400 0800 5511 5549 4000 " ++
		    "312e 322e 3832 362e 302e 312e 3336 3830 " ++
		    "3034 332e 322e 3131 3235 2e31 2e39 3231 " ++
		    "3233 3239 3735 3531 3830 3030 3139 3436 " ++
		    "3030 3431 3337 3739 3233 3335 3837 3634 " ++
		    "feff 0de0 0000 0000 feff dde0 0000 0000 " ++
		    "2000 0e00 5549 4000 312e 322e 3832 362e " ++
		    "302e 312e 3336 3830 3034 332e 322e 3131 " ++
		    "3235 2e31 2e37 3333 3734 3339 3534 3538 " ++
		    "3732 3134 3336 3235 3535 3635 3239 3132 " ++
		    "3533 3133 3934 3434 feff 0de0 0000 0000 " ++
		    "feff dde0 0000 0000 0102                "),
    {ok, Flow} = wolfpacs_flow:start_link(),

    {ok, Decoded, Rest} = decode(Flow, {explicit, little}, Encoded),
    [ ?_assertEqual(Decoded, Correct)
    , ?_assertEqual(Rest, <<1, 2>>)
    ].
