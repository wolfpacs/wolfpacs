
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
    case wolfpacs_vr_sq_item:decode(Flow, Strategy, Data) of
	{ok, Item, Rest = <<NewItem:32/bitstring, _/binary>>} ->
	    decode_item(Flow, Strategy, [Item|Items], Rest);
	{ok, Item, Rest} ->
	    {ok, [Item|Items], Rest};
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode item"),
	    wolfpacs_flow:expected_32(Flow, ?MODULE, Data),
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
