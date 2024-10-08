%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
%%
%% @doc Data Element
%%
%% http://dicom.nema.org/medical/dicom/2014c/output/chtml/part05/chapter_7.html#sect_7.1.1
%% Command Dictionary is always implicit
%% http://dicom.nema.org/dicom/2013/output/chtml/part07/sect_6.3.html
%%
%% The headers in a DICOM file is unfortunately not always know before hand.
%% Especially the pixel data header might be OB or OB.
%% When the data is explicitly encoded it is easy, however, for implicitly
%% encoded/decoded data it becomes trickier. Because of this, we need
%% to use the "Extra" map. Where we accumulate information about the
%% dataset while we are encoding/decoding.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_element).
-export([encode/6,
	 encode/7,
	 decode/3,
	 decode/4]).
-include("wolfpacs_types.hrl").

%%-------------------------------------------------------------------
%% @doc Encodes a Data Element without extra information
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(flow(), strategy(), integer(), integer(), string(), any()) -> binary().
encode(Flow, Strategy, G, E, VR, Bytes) ->
    encode(Flow, Strategy, G, E, VR, Bytes, #{}).

%%-------------------------------------------------------------------
%% @doc Encodes a Data Element
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(flow(), strategy(), integer(), integer(), string(), any(), map()) -> binary().
encode(Flow, {explicit, _Endian}, 0, E, VR, Bytes, Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "command group is always implicit little"),
    wolfpacs_data_element:encode(Flow, {implicit, little}, 0, E, VR, Bytes, Extra);
encode(Flow, Strategy, G, E, VR, <<>>, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "Empty data is always empty"),
    encode_common(Strategy, G, E, VR, <<>>);
encode(Flow, Strategy, G, 0, "UN", Bytes, Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "group lengths are always UL"),
    encode(Flow, Strategy, G, 0, "UL", Bytes, Extra);
encode(Flow, Strategy, G, E, "OB", Bytes, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OB"),
    encode_common(Strategy, G, E, "OB", wolfpacs_vr_ob:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "OW", Bytes, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OW"),
    encode_common(Strategy, G, E, "OW", wolfpacs_vr_ow:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "OF", Bytes, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OF"),
    encode_common(Strategy, G, E, "OF", wolfpacs_vr_of:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "OD", Bytes, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OD"),
    encode_common(Strategy, G, E, "OD", wolfpacs_vr_od:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "AE", Title, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode AE"),
    encode_common(Strategy, G, E, "AE", wolfpacs_vr_ae:encode(Flow, Strategy, Title));
encode(Flow, Strategy, G, E, "AT", AT, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode AT"),
    encode_common(Strategy, G, E, "AT", wolfpacs_vr_at:encode(Flow, Strategy, AT));
encode(Flow, Strategy, G, E, "UI", Title, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode UI"),
    encode_common(Strategy, G, E, "UI", wolfpacs_vr_ui:encode(Flow, Strategy, Title));
encode(Flow, Strategy, G, E, "US", US, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode US"),
    encode_common(Strategy, G, E, "US", wolfpacs_vr_us:encode(Flow, Strategy, US));
encode(Flow, Strategy, G, E, "UL", UL, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode UL"),
    encode_common(Strategy, G, E, "UL", wolfpacs_vr_ul:encode(Flow, Strategy, UL));
encode(Flow, Strategy, G, E, "PN", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode PN"),
    encode_common(Strategy, G, E, "PN", wolfpacs_vr_pn:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "LO", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode LO"),
    encode_common(Strategy, G, E, "LO", wolfpacs_vr_lo:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "UN", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode UN"),
    encode_common(Strategy, G, E, "UN", wolfpacs_vr_un:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "CS", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode CS"),
    encode_common(Strategy, G, E, "CS", wolfpacs_vr_cs:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "SH", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode SH"),
    encode_common(Strategy, G, E, "SH", wolfpacs_vr_sh:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "DA", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode DA"),
    encode_common(Strategy, G, E, "DA", wolfpacs_vr_da:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "TM", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode TM"),
    encode_common(Strategy, G, E, "TM", wolfpacs_vr_tm:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "DT", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode DT"),
    encode_common(Strategy, G, E, "DT", wolfpacs_vr_dt:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "ST", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode ST"),
    encode_common(Strategy, G, E, "ST", wolfpacs_vr_st:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "IS", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode IS"),
    encode_common(Strategy, G, E, "IS", wolfpacs_vr_is:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "AS", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode AS"),
    encode_common(Strategy, G, E, "AS", wolfpacs_vr_as:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "DS", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode DS"),
    encode_common(Strategy, G, E, "DS", wolfpacs_vr_ds:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "SS", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode SS"),
    encode_common(Strategy, G, E, "SS", wolfpacs_vr_ss:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "LT", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode LT"),
    encode_common(Strategy, G, E, "LT", wolfpacs_vr_lt:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "SL", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode SL"),
    encode_common(Strategy, G, E, "SL", wolfpacs_vr_sl:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "FD", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode FD"),
    encode_common(Strategy, G, E, "FD", wolfpacs_vr_fd:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "FL", Name, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode FL"),
    encode_common(Strategy, G, E, "FL", wolfpacs_vr_fl:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "SQ", Bytes, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OB"),
    encode_common(Strategy, G, E, "SQ", wolfpacs_vr_sq:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "xs", Value, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode xs"),
    encode_common(Strategy, G, E, "US", wolfpacs_vr_xs:encode(Flow, Strategy, Value));
encode(Flow, Strategy, G, E, VR, Bytes, Extra) ->
    case maps:get({G, E}, Extra, missing) of
	missing ->
	    logger:warning("[DataElement] Unable to encode (~p, ~p): ~p", [G, E, VR]),
	    wolfpacs_flow:failed(Flow, ?MODULE, io_lib:format("unable to pick encoder ~p ~p ~p", [G, E, VR])),
	    <<>>;
	RecoveredVR ->
	    encode(Flow, Strategy, G, E, RecoveredVR, Bytes, Extra)
    end.

%%-------------------------------------------------------------------
%% @doc Decode a Data Element without extra information
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(flow(), strategy(), binary()) -> {ok, {{integer(), integer()}, any()}, binary()} | error.
decode(Flow, Strategy, Data) ->
    decode(Flow, Strategy, Data, #{}).

%%-------------------------------------------------------------------
%% @doc Decode a Data Element
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(flow(), strategy(), binary(), map()) -> {ok, {{integer(), integer()}, any()}, binary()} | error.
decode(Flow, {explicit, _Endian}, Data = <<0:16, _/binary>>, Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "command group is always implicit little"),
    decode(Flow, {implicit, little}, Data, Extra);

decode(Flow, {Type, little}, <<G:16/little, E:16/little, Data/binary>>, Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode little"),
    wolfpacs_flow:ge(Flow, ?MODULE, G, E),
    decode_correct_vr(Flow, {Type, little}, G, E, Data, Extra);

decode(Flow, {Type, big}, <<G:16/big, E:16/big, Data/binary>>, Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode big"),
    wolfpacs_flow:ge(Flow, ?MODULE, G, E),
    decode_correct_vr(Flow, {Type, big}, G, E, Data, Extra);

decode(Flow, _, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to handle strategy"),
    error.

%%==============================================================================
%% Private Encoders
%%==============================================================================

%% Implicit strategy
-spec encode_common(strategy(), integer(), integer(), list(), binary()) -> binary().
encode_common({implicit, little}, G, E, _, Bytes) ->
    NbBytes = byte_size(Bytes),
    <<G:16/little, E:16/little, NbBytes:32/little, Bytes/binary>>;
encode_common({implicit, big}, G, E, _, Bytes) ->
    NbBytes = byte_size(Bytes),
    <<G:16/big, E:16/big, NbBytes:32/big, Bytes/binary>>;

%% Special Explicit Case
encode_common({explicit, Endian}, G, E, "OB", Data) ->
    encode_common_long({explicit, Endian}, G, E, "OB", Data);
encode_common({explicit, Endian}, G, E, "OW", Data) ->
    encode_common_long({explicit, Endian}, G, E, "OW", Data);
encode_common({explicit, Endian}, G, E, "OF", Data) ->
    encode_common_long({explicit, Endian}, G, E, "OF", Data);
encode_common({explicit, Endian}, G, E, "OD", Data) ->
    encode_common_long({explicit, Endian}, G, E, "OD", Data);
encode_common({explicit, Endian}, G, E, "UN", Data) ->
    encode_common_long({explicit, Endian}, G, E, "UN", Data);
encode_common({explicit, Endian}, G, E, "SQ", Data) ->
    encode_common_long({explicit, Endian}, G, E, "SQ", Data);

%% Normal Explicit Case
encode_common({explicit, Endian}, G, E, VR, Data) ->
    encode_common_short({explicit, Endian}, G, E, VR, Data).

-spec encode_common_long(strategy(), integer(), integer(), list(), binary()) -> binary().
encode_common_long({explicit, little}, G, E, VRTag, Data) ->
    Len = byte_size(Data),
    VR = list_to_binary(VRTag),
    <<G:16/little, E:16/little, VR/binary, 0:16, Len:32/little, Data/binary>>;
encode_common_long({explicit, big}, G, E, VRTag, Data) ->
    Len = byte_size(Data),
    VR = list_to_binary(VRTag),
    <<G:16/big, E:16/big, VR/binary, 0:16, Len:32/big, Data/binary>>.


-spec encode_common_short(strategy(), integer(), integer(), list(), binary()) -> binary().
encode_common_short({explicit, little}, G, E, VRTag, Data) ->
    Len = byte_size(Data),
    VR = list_to_binary(VRTag),
    <<G:16/little, E:16/little, VR/binary, Len:16/little, Data/binary>>;
encode_common_short({explicit, big}, G, E, VRTag, Data) ->
    Len = byte_size(Data),
    VR = list_to_binary(VRTag),
    <<G:16/big, E:16/big, VR/binary, Len:16/big, Data/binary>>.

%%==============================================================================
%% Private Decoders
%%==============================================================================

%%-------------------------------------------------------------------
%% @doc Decode vr
%%
%% @end
%%-------------------------------------------------------------------
decode_correct_vr(Flow, Strategy={implicit, _}, G, E, Data, Extra) ->
    VR = wolfpacs_group_elements:vr(G, E, Extra),
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr {implicit, little}"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, VR, Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"OB", _:16, Data/binary>>, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    wolfpacs_group_elements_cache:add(G, E, "OB"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "OB", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"OW", _:16, Data/binary>>, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    wolfpacs_group_elements_cache:add(G, E, "OW"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "OW", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"OF", _:16, Data/binary>>, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    wolfpacs_group_elements_cache:add(G, E, "OF"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "OF", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"OD", _:16, Data/binary>>, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    wolfpacs_group_elements_cache:add(G, E, "OD"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "OD", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"UN", _:16, Data/binary>>, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    wolfpacs_group_elements_cache:add(G, E, "UN"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "UN", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"SQ", _:16, Data/binary>>, _Extra) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    wolfpacs_group_elements_cache:add(G, E, "SQ"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "SQ", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<VRTag:16/bitstring, Data/binary>>, Extra) ->
    VR = case maps:get({G, E}, Extra, missing) of
	     missing ->
		 binary_to_list(VRTag);
	     FoundVR ->
		 FoundVR
	 end,
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    wolfpacs_group_elements_cache:add(G, E, VR),
    decode_with_vr_16bit_length(Flow, Strategy, G, E, VR, Data);

decode_correct_vr(Flow, _Strategy, G, E, _Data, _Extra) ->
    wolfpacs_flow:failed(Flow, ?MODULE, io_lib:format("unsupported VR for ~p ~p", [G, E])),
    error.

%%-------------------------------------------------------------------
%% @doc Decode vr with 32 bit length
%%
%% @end
%%-------------------------------------------------------------------
decode_with_vr_32bit_length(Flow, Strategy={_, little}, G, E, VR, <<Len:32/little, Data/binary>>) ->
    decode_common(Flow, Strategy, G, E, VR, Len, Data);
decode_with_vr_32bit_length(Flow, Strategy={_, big}, G, E, VR, <<Len:32/big, Data/binary>>) ->
    decode_common(Flow, Strategy, G, E, VR, Len, Data);
decode_with_vr_32bit_length(Flow, _Strategy, _G, _E, _VR, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode with 32bit length"),
    error.

%%-------------------------------------------------------------------
%% @doc Decode vr with 16 bit length
%%
%% @end
%%-------------------------------------------------------------------
decode_with_vr_16bit_length(Flow, Strategy={_, little}, G, E, VR, <<Len:16/little, Data/binary>>) ->
    decode_common(Flow, Strategy, G, E, VR, Len, Data);
decode_with_vr_16bit_length(Flow, Strategy={_, big}, G, E, VR, <<Len:16/big, Data/binary>>) ->
    decode_common(Flow, Strategy, G, E, VR, Len, Data);
decode_with_vr_16bit_length(Flow, _Strategy, _G, _E, _VR, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode with 16bit length"),
    error.

%%-------------------------------------------------------------------
%% @doc Decode Common With Decoder
%%
%% @end
%%-------------------------------------------------------------------
decode_common_with_decoder(Flow, _Strategy, G, E, 0, Data, _Decoder) ->
    wolfpacs_flow:good(Flow, ?MODULE, io_lib:format("common_with_decoder (~.16B, ~.16B) empty", [G, E])),
    {ok, {{G, E}, <<>>}, Data};
decode_common_with_decoder(Flow, Strategy={_, little}, G, E, 16#ffffffff, Data, Decoder) ->
    wolfpacs_flow:good(Flow, ?MODULE, io_lib:format("common_with_decoder (~.16B, ~.16B) unknown length", [G, E])),
    Len = byte_size(Data),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, Decoder);
decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, Decoder) ->
    wolfpacs_flow:good(Flow, ?MODULE, io_lib:format("common_with_decoder (~.16B, ~.16B)", [G, E])),
    case wolfpacs_utils:split(Data, Len) of
	{ok, Bytes, Rest2} ->
	    case Decoder:decode(Flow, Strategy, Bytes) of
		{ok, Value, Rest1} ->
		    wolfpacs_flow:success(Flow, ?MODULE),
		    Tag = io_lib:format("(~.16B, ~.16B)", [G, E]),
		    wolfpacs_flow:good(Flow, ?MODULE, Tag),

		    {ok, {{G, E}, Value}, <<Rest1/binary, Rest2/binary>>};
		_ ->
		    wolfpacs_flow:failed(Flow, ?MODULE, "decoder failed"),
		    wolfpacs_flow:failed(Flow, ?MODULE, Bytes),
		    error
	    end;
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, io_lib:format("not enough data ~.16B < ~.16B", [byte_size(Data), Len])),
	    error
    end.

%%-------------------------------------------------------------------
%% @doc Decode Common
%%
%% @end
%%-------------------------------------------------------------------
decode_common(Flow, Strategy, G, E, "OB", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common OB"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_ob);

decode_common(Flow, Strategy, G, E, "OW", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common OW"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_ow);

decode_common(Flow, Strategy, G, E, "OF", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common OF"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_of);

decode_common(Flow, Strategy, G, E, "OD", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common OD"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_od);

decode_common(Flow, Strategy, G, E, "PN", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common PN"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_pn);

decode_common(Flow, Strategy, G, E, "LO", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common LO"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_lo);

decode_common(Flow, Strategy, G, E, "AE", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common AE"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_ae);

decode_common(Flow, Strategy, G, E, "AT", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common AT"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_at);

decode_common(Flow, Strategy, G, E, "UI", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common UI"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_ui);

decode_common(Flow, Strategy, G, E, "US", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common US"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_us);

decode_common(Flow, Strategy, G, E, "UL", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common UL"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_ul);

decode_common(Flow, Strategy, G, E, "CS", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common CS"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_cs);

decode_common(Flow, Strategy, G, E, "SH", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common SH"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_sh);

decode_common(Flow, Strategy, G, E, "UN", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common UN"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_un);

decode_common(Flow, Strategy, G, E, "DA", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common DA"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_da);

decode_common(Flow, Strategy, G, E, "TM", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common TM"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_tm);

decode_common(Flow, Strategy, G, E, "DT", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common DT"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_dt);

decode_common(Flow, Strategy, G, E, "ST", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common ST"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_st);

decode_common(Flow, Strategy, G, E, "IS", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common IS"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_is);

decode_common(Flow, Strategy, G, E, "AS", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common AS"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_as);

decode_common(Flow, Strategy, G, E, "DS", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common DS"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_ds);

decode_common(Flow, Strategy, G, E, "SS", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common SS"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_ss);

decode_common(Flow, Strategy, G, E, "xs", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common xs"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_xs);

decode_common(Flow, Strategy, G, E, "LT", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common LT"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_lt);

decode_common(Flow, Strategy, G, E, "SL", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common SL"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_sl);

decode_common(Flow, Strategy, G, E, "FD", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common FD"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_fd);

decode_common(Flow, Strategy, G, E, "FL", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common FL"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_fl);

decode_common(Flow, Strategy, G, E, "SQ", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common SQ"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_sq);

decode_common(Flow, _Strategy, G, E, VR, _Len, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, io_lib:format("Unable to decode (~p, ~p) ~p", [G, E, VR])),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

%%                _____                      _                 _
%%               |  ___| __ ___  _ __ ___   | |__   ___   ___ | | __
%%               | |_ | '__/ _ \| '_ ` _ \  | '_ \ / _ \ / _ \| |/ /
%%               |  _|| | | (_) | | | | | | | |_) | (_) | (_) |   <
%%               |_|  |_|  \___/|_| |_| |_| |_.__/ \___/ \___/|_|\_\
%%

encode_book_explicit_little_example_test() ->
    %% Page 51
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe", 32>>,
    ?assertEqual(encode(Flow, Strategy, 16, 16, "PN", <<"Smith^Joe">>), TestData).

decode_book_explicit_little_example_test() ->
    %% Page 51
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe", 32>>,
    Correct = {{16, 16}, <<"Smith^Joe">>},
    ?assertEqual(decode(Flow, Strategy, TestData), {ok, Correct, <<>>}).

encode_book_implicit_little_example_test() ->
    %% Page 51
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {implicit, little},
    TestData = <<16, 0, 16, 0, 10, 0, 0, 0, "Smith^Joe", 32>>,
    ?assertEqual(encode(Flow, Strategy, 16, 16, "PN", <<"Smith^Joe">>), TestData).

decode_book_implicit_little_example_test() ->
    %% Page 51
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {implicit, little},
    TestData = <<16, 0, 16, 0, 10, 0, 0, 0, "Smith^Joe", 32>>,
    Correct = {{16, 16}, <<"Smith^Joe">>},
    ?assertEqual(decode(Flow, Strategy, TestData), {ok, Correct, <<>>}).

%%                       __  __ _       _                 _
%%                      |  \/  (_)_ __ (_)_ __ ___   __ _| |
%%                      | |\/| | | '_ \| | '_ ` _ \ / _` | |
%%                      | |  | | | | | | | | | | | | (_| | |
%%                      |_|  |_|_|_| |_|_|_| |_| |_|\__,_|_|
%%

encode_missing_vr_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    ?assertEqual(encode(Flow, {explicit, little}, 1, 0, "XY", 123), <<>>).

minimal_encode_decode_1_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    Data = [1, 2, 3, 4],
    Correct = {{100, 200}, Data},
    Encoded0 = encode(Flow, Strategy, 100, 200, "OB", Data),
    ?assertEqual(decode(Flow, Strategy, Encoded0), {ok, Correct, <<>>}).

minimal_encode_decode_2_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, big},
    Data = [1, 2, 3, 4],
    Correct = {{100, 200}, Data},
    Encoded0 = encode(Flow, Strategy, 100, 200, "OB", Data),
    ?assertEqual(decode(Flow, Strategy, Encoded0), {ok, Correct, <<>>}).

minimal_encode_decode_3_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {implicit, little},
    Data = [1, 2, 3, 4],
    {Group, Element} = wolfpacs_group_elements:vr_to_example_group_element("OB"),
    Correct = {{Group, Element}, Data},
    Encoded0 = encode(Flow, Strategy, Group, Element, "OB", Data),
    ?assertEqual(decode(Flow, Strategy, Encoded0), {ok, Correct, <<>>}).

minimal_encode_decode_4_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {implicit, big},
    Data = [1, 2, 3, 4],
    {Group, Element} = wolfpacs_group_elements:vr_to_example_group_element("OB"),
    Correct = {{Group, Element}, Data},
    Encoded0 = encode(Flow, Strategy, Group, Element, "OB", Data),
    ?assertEqual(decode(Flow, Strategy, Encoded0), {ok, Correct, <<>>}).

%%                 _____      _                     _   _
%%                | ____|_  _| |__   __ _ _   _ ___| |_(_)_   _____
%%                |  _| \ \/ / '_ \ / _` | | | / __| __| \ \ / / _ \
%%                | |___ >  <| | | | (_| | |_| \__ \ |_| |\ V /  __/
%%                |_____/_/\_\_| |_|\__,_|\__,_|___/\__|_| \_/ \___|
%%

encode_decode_ob_test_() ->
    encode_decode_common("OB", [1, 2, 3, 4, 5]).

encode_decode_ow_test_() ->
    encode_decode_common("OW", [1, 2, 3, 4, 5]).

encode_decode_of_test_() ->
    encode_decode_common("OF", [1.0, 2.0, 3.0, 4.0, 5.0]).

encode_decode_od_test_() ->
    encode_decode_common("OD", [1.0, 2.0, 3.0, 4.0, 5.0]).

encode_decode_pn_test_() ->
    encode_decode_common("PN", <<"Smith^Joe">>).

encode_decode_lo_test_() ->
    encode_decode_common("LO", <<"This is a long description">>).

encode_decode_ae_test_() ->
    encode_decode_common("AE", <<"AE1">>).

encode_decode_sh_test_() ->
    encode_decode_common("SH", <<"SH-STRING">>).

encode_decode_at_test_() ->
    encode_decode_common("AT", {16#18, 16#ff}).

encode_decode_un_test_() ->
    encode_decode_common("UN", <<"UN1">>).

encode_decode_cs_test_() ->
    encode_decode_common("CS", <<"CS1">>).

encode_decode_ui_test_() ->
    encode_decode_common("UI", <<"1.2.3">>).

encode_decode_us_test_() ->
    encode_decode_common("US", 1024).

encode_decode_ul_test_() ->
    encode_decode_common("UL", 1024).

encode_decode_da_test_() ->
    encode_decode_common("DA", <<"20201011A">>).

encode_decode_tm_test_() ->
    encode_decode_common("TM", <<"20201011B">>).

encode_decode_dt_test_() ->
    encode_decode_common("DT", <<"20201011C">>).

encode_decode_st_test_() ->
    encode_decode_common("ST", <<"20201011D">>).

encode_decode_is_test_() ->
    encode_decode_common("IS", 123).

encode_decode_as_test_() ->
    encode_decode_common("AS", <<"123D">>).

encode_decode_ds_test_() ->
    encode_decode_common("DS", <<"20201011G">>).

encode_decode_ss_test_() ->
    encode_decode_common("SS", -128).

encode_decode_lt_test_() ->
    encode_decode_common("LT", <<"abcd">>).

encode_decode_sl_test_() ->
    encode_decode_common("SL", -1024).

encode_decode_fl_test_() ->
    encode_decode_common("FL", -12000.0).

encode_decode_fd_test_() ->
    encode_decode_common("FD", -12000.0).

encode_decode_common(VR, Data) ->
    lists:flatten(
      [ encode_decode_common({explicit, little}, VR, Data)
      , encode_decode_common({explicit, big}, VR, Data)
      , encode_decode_common({implicit, little}, VR, Data)
      , encode_decode_common({implicit, big}, VR, Data)
      ]).

encode_decode_common(Strategy, VR, Data) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    {G, E} = wolfpacs_group_elements:vr_to_example_group_element(VR),
    Encoded0 = encode(Flow, Strategy, G, E, VR, Data),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1>>,

    [?_assertEqual(decode(Flow, Strategy, Encoded0), {ok, {{G, E}, Data}, <<>>}),
     ?_assertEqual(decode(Flow, Strategy, Encoded1), {ok, {{G, E}, Data}, <<42>>}),
     ?_assertEqual(decode(Flow, Strategy, Incorrect0), error),
     ?_assertEqual(decode(Flow, Strategy, Incorrect1), error)].

decode_example_one_test() ->
    %% (0070,0060) SQ (Sequence with explicit length #=1)      #  62, 1 GraphicLayerSequence
    %%   (fffe,e000) na (Item with explicit length #=3)          #  54, 1 Item
    %%     (0070,0002) CS [ANNOTATION LAYER]                       #  16, 1 GraphicLayer
    %%     (0070,0062) IS [1]                                      #   2, 1 GraphicLayerOrder
    %%     (0070,0068) LO [Annotations]                            #  12, 1 GraphicLayerDescription
    %%   (fffe,e00d) na (ItemDelimitationItem for re-encoding)   #   0, 0 ItemDelimitationItem
    %% (fffe,e0dd) na (SequenceDelimitationItem for re-encod.) #   0, 0 SequenceDelimitationItem

    Correct = [#{{112,2} => <<"ANNOTATION LAYER">>,
		{112,98} => 1,
		{112,104} => <<"Annotations">>}],

    Encoded = wolfpacs_utils:hexl_log_to_binary(
		"" ++
		    "7000 6000 5351 0000 3e00 0000 " ++
		    "feff 00e0 3600 0000 7000 0200 4353 1000 " ++
		    "414e 4e4f 5441 5449 4f4e 204c 4159 4552 " ++
		    "7000 6200 4953 0200 3120 7000 6800 4c4f " ++
		    "0c00 416e 6e6f 7461 7469 6f6e 7320"),

    {ok, Flow} = wolfpacs_flow:start_link(),
    Decoded = decode(Flow, {explicit, little}, Encoded),
    ?assertEqual(Decoded, {ok, {{16#0070, 16#0060}, Correct}, <<>>}).

nested_sq_explicit_little_test_() ->
    G = 16#0008,
    E = 16#1115,

    SOPClassUID = <<"1.2.840.10008.5.1.4.1.1.4">>,
    SOPInstance = <<"1.2.826.0.1.3680043.2.1125.1.92123297551800019460041377923358764">>,
    SeriesUID = <<"1.2.826.0.1.3680043.2.1125.1.73374395458721436255565291253139444">>,


    Correct = {{G, E}, [#{{8,4416} => [#{{8,4432} => SOPClassUID,
					 {8,4437} => SOPInstance}],
			  {32,14} => SeriesUID
			 }]},

    Encoded = wolfpacs_utils:hexl_log_to_binary(
		"" ++
		    "                                   0800 " ++
		    "1511 5351 0000 ffff ffff feff 00e0 ffff " ++
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
