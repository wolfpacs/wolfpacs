%%%-------------------------------------------------------------------
%% @doc Data Element
%%
%% http://dicom.nema.org/medical/dicom/2014c/output/chtml/part05/chapter_7.html#sect_7.1.1
%% Command Dictionary is always implicit
%% http://dicom.nema.org/dicom/2013/output/chtml/part07/sect_6.3.html
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_element).
-export([encode/6,
	 decode/3]).
-include("wolfpacs_types.hrl").

%%-------------------------------------------------------------------
%% @doc Encodes a Data Element
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(pid(), strategy(), integer(), integer(), string(), any()) -> binary().
encode(Flow, {explicit, _Endian}, 0, E, VR, Bytes) ->
    wolfpacs_flow:good(Flow, ?MODULE, "command group is always implicit little"),
    wolfpacs_data_element:encode(Flow, {implicit, little}, 0, E, VR, Bytes);
encode(Flow, Strategy, G, E, "OB", Bytes) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OB"),
    encode_common(Strategy, G, E, "OB", wolfpacs_vr_ob:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "OW", Bytes) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OW"),
    encode_common(Strategy, G, E, "OW", wolfpacs_vr_ow:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "OF", Bytes) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OF"),
    encode_common(Strategy, G, E, "OF", wolfpacs_vr_of:encode(Flow, Strategy, Bytes));
encode(Flow, Strategy, G, E, "AE", Title) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode AE"),
    encode_common(Strategy, G, E, "AE", wolfpacs_vr_ae:encode(Flow, Strategy, Title));
encode(Flow, Strategy, G, E, "AT", AT) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode AT"),
    encode_common(Strategy, G, E, "AT", wolfpacs_vr_at:encode(Flow, Strategy, AT));
encode(Flow, Strategy, G, E, "UI", Title) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode UI"),
    encode_common(Strategy, G, E, "UI", wolfpacs_vr_ui:encode(Flow, Strategy, Title));
encode(Flow, Strategy, G, E, "US", US) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode US"),
    encode_common(Strategy, G, E, "US", wolfpacs_vr_us:encode(Flow, Strategy, US));
encode(Flow, Strategy, G, E, "UL", UL) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode UL"),
    encode_common(Strategy, G, E, "UL", wolfpacs_vr_ul:encode(Flow, Strategy, UL));
encode(Flow, Strategy, G, E, "PN", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode PN"),
    encode_common(Strategy, G, E, "PN", wolfpacs_vr_pn:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "LO", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode LO"),
    encode_common(Strategy, G, E, "LO", wolfpacs_vr_lo:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "UN", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode UN"),
    encode_common(Strategy, G, E, "UN", wolfpacs_vr_un:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "CS", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode CS"),
    encode_common(Strategy, G, E, "CS", wolfpacs_vr_cs:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "SH", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode SH"),
    encode_common(Strategy, G, E, "SH", wolfpacs_vr_sh:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "DA", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode DA"),
    encode_common(Strategy, G, E, "DA", wolfpacs_vr_da:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "TM", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode TM"),
    encode_common(Strategy, G, E, "TM", wolfpacs_vr_tm:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "DT", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode DT"),
    encode_common(Strategy, G, E, "DT", wolfpacs_vr_dt:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "ST", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode ST"),
    encode_common(Strategy, G, E, "ST", wolfpacs_vr_st:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "IS", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode IS"),
    encode_common(Strategy, G, E, "IS", wolfpacs_vr_is:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "AS", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode AS"),
    encode_common(Strategy, G, E, "AS", wolfpacs_vr_as:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "DS", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode DS"),
    encode_common(Strategy, G, E, "DS", wolfpacs_vr_ds:encode(Flow, Strategy, Name));
encode(Flow, Strategy, G, E, "SS", Name) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode SS"),
    encode_common(Strategy, G, E, "SS", wolfpacs_vr_ss:encode(Flow, Strategy, Name));

encode(Flow, Strategy, G, E, "SQ", Bytes) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode OB"),
    encode_common(Strategy, G, E, "SQ", wolfpacs_vr_sq:encode(Flow, Strategy, Bytes));

encode(Flow, _Strategy, _G, _E, "ox", _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to encode ox (OB or OW)"),
    <<>>;
encode(Flow, _Strategy, G, E, VRTag, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, io_lib:format("unable to encode ~p ~p ~p", [G, E, VRTag])),
    <<>>.

%%-------------------------------------------------------------------
%% @doc Decode a Data Element
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(pid(), strategy(), binary()) -> {ok, {{integer(), integer()}, any()}, binary()} | {error, binary(), list(string())}.
decode(Flow, {explicit, _Endian}, Data = <<0:16, _/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "command group is always implicit little"),
    decode(Flow, {implicit, little}, Data);

decode(Flow, {Type, little}, <<G:16/little, E:16/little, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode little"),
    decode_correct_vr(Flow, {Type, little}, G, E, Data);

decode(Flow, {Type, big}, <<G:16/big, E:16/big, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode big"),
    decode_correct_vr(Flow, {Type, big}, G, E, Data);

decode(Flow, _, _) ->
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
decode_correct_vr(Flow, Strategy={implicit, _}, G, E, Data) ->
    VR = wolfpacs_group_elements:vr(G, E),
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr {implicit, little}"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, VR, Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"OB", _:16, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "OB", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"OW", _:16, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "OW", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"OF", _:16, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "OF", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<"SQ", _:16, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    decode_with_vr_32bit_length(Flow, Strategy, G, E, "SQ", Data);

decode_correct_vr(Flow, Strategy={explicit, _}, G, E, <<VRTag:16/bitstring, Data/binary>>) ->
    VR = binary_to_list(VRTag),
    wolfpacs_flow:good(Flow, ?MODULE, "decode_correct_vr"),
    decode_with_vr_16bit_length(Flow, Strategy, G, E, VR, Data);

decode_correct_vr(Flow, _Strategy, G, E, _Data) ->
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
decode_common_with_decoder(Flow, _Strategy, G, E, 16#ffffffff, Data, _Decoder) ->
    Len = byte_size(Data),
    decode_common_with_decoder(Flow, _Strategy, G, E, Len, Data, _Decoder);
decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, Decoder) ->
    case wolfpacs_utils:split(Data, Len) of
	{ok, Bytes, Rest} ->
	    case Decoder:decode(Flow, Strategy, Bytes) of
		{ok, Value, <<>>} ->
		    wolfpacs_flow:success(Flow, ?MODULE),
		    Tag = io_lib:format("(~.16B, ~.16B)", [G, E]),
		    wolfpacs_flow:good(Flow, ?MODULE, {Tag, Value}),

		    {ok, {{G, E}, Value}, Rest};
		_ ->
		    wolfpacs_flow:failed(Flow, ?MODULE, "decoder failed"),
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

decode_common(Flow, Strategy, G, E, "SQ", Len, Data) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode_common SQ"),
    decode_common_with_decoder(Flow, Strategy, G, E, Len, Data, wolfpacs_vr_sq);

decode_common(Flow, _, _G, _E, VR, _Len, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, {"unsupported VR", VR}),
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
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe", 0>>,
    ?assertEqual(encode(Flow, Strategy, 16, 16, "PN", "Smith^Joe"), TestData).

decode_book_explicit_little_example_test() ->
    %% Page 51
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe", 0>>,
    Correct = {{16, 16}, <<"Smith^Joe">>},
    ?assertEqual(decode(Flow, Strategy, TestData), {ok, Correct, <<>>}).

encode_book_implicit_little_example_test() ->
    %% Page 51
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {implicit, little},
    TestData = <<16, 0, 16, 0, 10, 0, 0, 0, "Smith^Joe", 0>>,
    ?assertEqual(encode(Flow, Strategy, 16, 16, "PN", "Smith^Joe"), TestData).

decode_book_implicit_little_example_test() ->
    %% Page 51
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {implicit, little},
    TestData = <<16, 0, 16, 0, 10, 0, 0, 0, "Smith^Joe", 0>>,
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
    encode_decode_common("OF", [1, 2, 3, 4, 5]).

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
    encode_decode_common("DA", <<"20201011">>).

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

encode_ox_test() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    ?assertEqual(encode(Flow, {explicit, little}, 1, 2, "ox", <<1, 2, 3, 4>>), <<>>).

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
