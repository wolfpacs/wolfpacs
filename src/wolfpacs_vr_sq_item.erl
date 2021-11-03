%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%
%% @doc Sequence Item
%%
%% PITFALL: I found out this the hard way. The delination tags should
%% not be read as a 32bit tag, but as a normal (gggg, eeee) tag.
%% Example, FFFE:16, E0DD:16 not FFFEE0DD:32
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

%%-------------------------------------------------------------------
%% @doc Decode length
%%
%% @end
%%-------------------------------------------------------------------
decode_length(Flow, Strategy, <<16#ffffffff:32, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "strip unknown length"),
    decode_with_length(Flow, Strategy, Data, byte_size(Data));
decode_length(Flow, Strategy={_, little}, <<Len:32/little, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, io_lib:format("strip length ~4.16B", [Len])),
    decode_with_length(Flow, Strategy, Data, Len);
decode_length(Flow, Strategy={_, big}, <<Len:32/big, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, io_lib:format("strip length ~4.16B", [Len])),
    decode_with_length(Flow, Strategy, Data, Len).

%%-------------------------------------------------------------------
%% @doc Decode with length
%%
%% @end
%%-------------------------------------------------------------------
decode_with_length(Flow, Strategy, AllData, Length) ->
    case wolfpacs_utils:split(AllData, Length) of
	{ok, Data, Rest2} ->
	    case decode_headers(Flow, Strategy, Data, []) of
		{ok, DataSet, Rest1} ->
		    {ok, DataSet, <<Rest1/binary, Rest2/binary>>};
		_ ->
		    error
	    end;
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "not enough data"),
	    error
    end.

%%-------------------------------------------------------------------
%% @doc Decode headers
%%
%% @end
%%-------------------------------------------------------------------
decode_headers(_Flow, _Strategy, <<>>, Acc) ->
    {ok, maps:from_list(Acc), <<>>};
decode_headers(_Flow, {_, little}, <<16#FFFE:16/little, 16#E00D:16/little, 0:32, Rest/binary>>, Acc) ->
    {ok, maps:from_list(Acc), Rest};
decode_headers(_Flow, {_, big}, <<16#FFFE:16/big, 16#E00D:16/big, 0:32, Rest/binary>>, Acc) ->
    {ok, maps:from_list(Acc), Rest};
decode_headers(_Flow, {_, little}, <<16#FFFE:16/little, 16#E0DD:16/little, 0:32, Rest/binary>>, Acc) ->
    {ok, maps:from_list(Acc), Rest};
decode_headers(_Flow, {_, big}, <<16#FFFE:16/big, 16#E0DD:16/big, 0:32, Rest/binary>>, Acc) ->
    {ok, maps:from_list(Acc), Rest};
decode_headers(Flow, Strategy, Data, Acc) ->
    wolfpacs_flow:good(Flow, ?MODULE, "next header"),
    case wolfpacs_data_element:decode(Flow, Strategy, Data) of
	{ok, Header, Rest} ->
	    wolfpacs_flow:good(Flow, ?MODULE, Header),
	    decode_headers(Flow, Strategy, Rest, [Header|Acc]);
	_ ->
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


decode_example_test_() ->
    Correct = #{ {8,4432} => <<"1.2.840.10008.5.1.4.1.1.4">>
	       , {8,4437} => <<"1.2.826.0.1.3680043.2.1125.1.92123297551800019460041377923358764">>
	       },

    Encoded = wolfpacs_utils:hexl_log_to_binary(
		"" ++
                    "                                   feff " ++
		    "00e0 ffff ffff 0800 5011 5549 1a00 312e " ++
		    "322e 3834 302e 3130 3030 382e 352e 312e " ++
		    "342e 312e 312e 3400 0800 5511 5549 4000 " ++
		    "312e 322e 3832 362e 302e 312e 3336 3830 " ++
		    "3034 332e 322e 3131 3235 2e31 2e39 3231 " ++
		    "3233 3239 3735 3531 3830 3030 3139 3436 " ++
		    "3030 3431 3337 3739 3233 3335 3837 3634 " ++
		    "feff 0de0 0000 0000 0102                "),
    {ok, Flow} = wolfpacs_flow:start_link(),
    {ok, DataSet, Rest} = decode(Flow, {explicit, little}, Encoded),
    [ ?_assertEqual(DataSet, Correct)
    , ?_assertEqual(Rest, <<1, 2>>)
    ].
