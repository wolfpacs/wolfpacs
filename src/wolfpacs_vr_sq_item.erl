%%%-------------------------------------------------------------------
%% @doc Sequence Item
%%
%% PITFALL: I found out this the hard way. The delination tags should
%% not be read as a 32bit tag, but as a normal (gggg, eeee) tag.
%% Example, <<FFFE:16, E0DD:16>> not <<FFFEE0DD:32>>
%%
%% PITFALL: There can be more than one element in the sequence.
%% Example:
%%
%% (0070,0009) SQ (Sequence with explicit length #=31)     # 3092, 1 GraphicObjectSequence
%%   (fffe,e000) na (Item with explicit length #=6)          #  84, 1 Item
%%     (0070,0005) CS [PIXEL]                                  #   6, 1 GraphicAnnotationUnits
%%     (0070,0020) US 2                                        #   2, 1 GraphicDimensions
%%     (0070,0021) US 2                                        #   2, 1 NumberOfGraphicPoints
%%     (0070,0022) FL 128\21\353\21                            #  16, 4 GraphicData
%%     (0070,0023) CS [POLYLINE]                               #   8, 1 GraphicType
%%     (0070,0024) CS [N]                                      #   2, 1 GraphicFilled
%%   (fffe,e00d) na (ItemDelimitationItem for re-encoding)   #   0, 0 ItemDelimitationItem
%%   (fffe,e000) na (Item with explicit length #=6)          #  84, 1 Item
%%     (0070,0005) CS [PIXEL]                                  #   6, 1 GraphicAnnotationUnits
%%     (0070,0020) US 2                                        #   2, 1 GraphicDimensions
%%     (0070,0021) US 2                                        #   2, 1 NumberOfGraphicPoints
%%     (0070,0022) FL 128\18\128\24                            #  16, 4 GraphicData
%%     (0070,0023) CS [POLYLINE]                               #   8, 1 GraphicType
%%     (0070,0024) CS [N]                                      #   2, 1 GraphicFilled
%%   (fffe,e00d) na (ItemDelimitationItem for re-encoding)   #   0, 0 ItemDelimitationItem
%%   ...
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_sq_item).
-export([encode/3, decode/3]).

%%-------------------------------------------------------------------
%% @doc Encodes
%%
%% @end
%%-------------------------------------------------------------------
encode(Flow, Strategy={_, little}, Items) ->
    Data = wolfpacs_data_elements:encode(Flow, Strategy, Items),
    Length = byte_size(Data),
    wolfpacs_flow:generated(Flow, ?MODULE, 4 + 4),
    <<16#fffe:16/little, 16#e000:16/little, Length:32/little, Data/binary>>;
encode(Flow, Strategy={_, big}, Items) ->
    Data = wolfpacs_data_elements:encode(Flow, Strategy, Items),
    Length = byte_size(Data),
    wolfpacs_flow:generated(Flow, ?MODULE, 4 + 4),
    <<16#fffe:16/big, 16#e000:16/big, Length:32/big, Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Encodes
%%
%% @end
%%-------------------------------------------------------------------
decode(Flow, Strategy={_, little}, <<16#fffe:16/little, 16#e000:16/little, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "strip item delimiter (little)"),
    decode_length(Flow, Strategy, Data);
decode(Flow, Strategy={_, big}, <<16#fffe:16/big, 16#e000:16/big, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "strip item delimiter (big)"),
    decode_length(Flow, Strategy, Data);
decode(Flow, _Strategy, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "missing item delimiter"),
    error.

%%==============================================================================
%% Private
%%==============================================================================

decode_length(Flow, Strategy, <<16#ffffffff:32, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "strip unknown length"),
    wolfpacs_data_elements:decode(Flow, Strategy, Data);
decode_length(Flow, Strategy={_, little}, <<Len:32/little, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, io_lib:format("strip length ~4.16B", [Len])),
    decode_with_length(Flow, Strategy, Data, Len);
decode_length(Flow, Strategy={_, big}, <<Len:32/big, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, io_lib:format("strip length ~4.16B", [Len])),
    decode_with_length(Flow, Strategy, Data, Len).

decode_with_length(Flow, Strategy, AllData, Length) ->
    case wolfpacs_utils:split(AllData, Length) of
	{ok, Data, Rest} ->
	    case wolfpacs_data_elements:decode(Flow, Strategy, Data) of
		{ok, Result, <<>>} ->
		    {ok, Result, Rest};
		_ ->
		    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode")
	    end;
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "not enough data"),
	    error
    end.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(Flow, Strategy) ->
    Group = 16#0040,
    Element = 16#A171,
    Data = <<"1.2.3.4.5">>,
    Encoded0 = encode(Flow, Strategy, [{{Group, Element}, Data}]),
    Encoded1 = <<Encoded0/binary, 42>>,
    Encoded2 = wolfpacs_utils:drop_last_byte(Encoded0),
    Encoded3 = wolfpacs_utils:drop_first_byte(Encoded0),
    Encoded4 = <<1, 2, 3, 4>>,

    Decoded0 = decode(Flow, Strategy, Encoded0),
    Decoded1 = decode(Flow, Strategy, Encoded1),
    Decoded2 = decode(Flow, Strategy, Encoded2),
    Decoded3 = decode(Flow, Strategy, Encoded3),
    Decoded4 = decode(Flow, Strategy, Encoded4),

    [ ?_assertEqual(Decoded0, {ok, #{{Group, Element} => Data}, <<>>})
    , ?_assertEqual(Decoded1, {ok, #{{Group, Element} => Data}, <<42>>})
    , ?_assertEqual(Decoded2, error)
    , ?_assertEqual(Decoded3, error)
    , ?_assertEqual(Decoded4, error)
    ].

encode_decode_explicit_little_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    encode_decode_common(Flow, {explicit, little}).

encode_decode_explicit_big_test_() ->
     {ok, Flow} = wolfpacs_flow:start_link(),
     encode_decode_common(Flow, {explicit, big}).

encode_decode_implicit_little_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    encode_decode_common(Flow, {implicit, little}).

encode_decode_implicit_big_test_() ->
     {ok, Flow} = wolfpacs_flow:start_link(),
     encode_decode_common(Flow, {implicit, big}).
