%%%-------------------------------------------------------------------
%% @doc Data Elements (Explicit Little).
%%
%% http://dicom.nema.org/medical/dicom/2014c/output/chtml/part05/chapter_7.html#sect_7.1.1
%% Command Dictionary is always implicit
%% http://dicom.nema.org/dicom/2013/output/chtml/part07/sect_6.3.html
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_element).
-export([encode/5,
	 decode/2]).
-include("wolfpacs_types.hrl").

%%-------------------------------------------------------------------
%% @doc Encodes a Data Element
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(strategy(), integer(), integer(), list(), any()) -> binary().
encode({explicit, _Endian}, 0, E, VR, Bytes) ->
    %% Command group is always implicit
    wolfpacs_data_element:encode({implicit, little}, 0, E, VR, Bytes);
encode(Strategy, G, E, "OB", Bytes) ->
    encode_common(Strategy, G, E, "OB", wolfpacs_vr_ob:encode(Strategy, Bytes));
encode(Strategy, G, E, "OW", Bytes) ->
    encode_common(Strategy, G, E, "OW", wolfpacs_vr_ow:encode(Strategy, Bytes));
encode(Strategy, G, E, "OF", Bytes) ->
    encode_common(Strategy, G, E, "OF", wolfpacs_vr_of:encode(Strategy, Bytes));
encode(Strategy, G, E, "AE", Title) ->
    encode_common(Strategy, G, E, "AE", wolfpacs_vr_ae:encode(Strategy, Title));
encode(Strategy, G, E, "UI", Title) ->
    encode_common(Strategy, G, E, "UI", wolfpacs_vr_ui:encode(Strategy, Title));
encode(Strategy, G, E, "US", US) ->
    encode_common(Strategy, G, E, "US", wolfpacs_vr_us:encode(Strategy, US));
encode(Strategy, G, E, "UL", UL) ->
    encode_common(Strategy, G, E, "UL", wolfpacs_vr_ul:encode(Strategy, UL));
encode(Strategy, G, E, "PN", Name) ->
    encode_common(Strategy, G, E, "PN", wolfpacs_vr_pn:encode(Strategy, Name));
encode(Strategy, G, E, "LO", Name) ->
    encode_common(Strategy, G, E, "LO", wolfpacs_vr_lo:encode(Strategy, Name));
encode(Strategy, G, E, "UN", Name) ->
    encode_common(Strategy, G, E, "UN", wolfpacs_vr_un:encode(Strategy, Name));
encode(Strategy, G, E, "CS", Name) ->
    encode_common(Strategy, G, E, "CS", wolfpacs_vr_cs:encode(Strategy, Name));
encode(Strategy, G, E, "DA", Name) ->
    encode_common(Strategy, G, E, "DA", wolfpacs_vr_da:encode(Strategy, Name));
encode(_Strategy, G, E, "ox", _) ->
    _ = lager:warning("[data_element_explicit] unable to encode ox (OB or OW)", [G, E]),
    <<>>;
encode(_Strategy, G, E, VRTag, _) ->
    _ = lager:warning("[data_element_explicit] unable to encode ~p ~p ~p", [G, E, VRTag]),
    VR = list_to_binary(VRTag),
    <<"error", VR/binary>>.

%%-------------------------------------------------------------------
%% @doc Decode a Data Element
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(strategy(), binary()) -> {ok, {{integer(), integer()}, any()}, binary()} | {error, binary(), list(string())}.
decode({explicit, _Endian}, OrgData = <<0:16, _/binary>>) ->
    %% Command group is always implicit
    wolfpacs_data_element:decode({implicit, little}, OrgData);

decode({Type, little}, OrgData = <<G:16/little, E:16/little, Data/binary>>) ->
    decode_correct_vr_and_length({Type, little}, OrgData, G, E, Data);

decode({Type, big}, OrgData = <<G:16/big, E:16/big, Data/binary>>) ->
    decode_correct_vr_and_length({Type, big}, OrgData, G, E, Data);

decode(_, OrgData) ->
    {error, OrgData, ["unable to handle strategy"]}.

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

decode_correct_vr_and_length({implicit, little}, OrgData, G, E, <<Len:32/little, Data/binary>>) ->
    VR = wolfpacs_group_elements:vr(G, E),
    decode_common({implicit, little}, OrgData, G, E, VR, Len, Data);

decode_correct_vr_and_length({implicit, big}, OrgData, G, E, <<Len:32/big, Data/binary>>) ->
    VR = wolfpacs_group_elements:vr(G, E),
    decode_common({implicit, big}, OrgData, G, E, VR, Len, Data);

decode_correct_vr_and_length({explicit, little}, OrgData, G, E, <<"OB", _:16, Len:32/little, Data/binary>>) ->
    decode_common({explicit, little}, OrgData, G, E, "OB", Len, Data);
decode_correct_vr_and_length({explicit, big}, OrgData, G, E, <<"OB", _:16, Len:32/big, Data/binary>>) ->
    decode_common({explicit, big}, OrgData, G, E, "OB", Len, Data);

decode_correct_vr_and_length({explicit, little}, OrgData, G, E, <<"OW", _:16, Len:32/little, Data/binary>>) ->
    decode_common({explicit, little}, OrgData, G, E, "OW", Len, Data);
decode_correct_vr_and_length({explicit, big}, OrgData, G, E, <<"OW", _:16, Len:32/big, Data/binary>>) ->
    decode_common({explicit, big}, OrgData, G, E, "OW", Len, Data);

decode_correct_vr_and_length({explicit, little}, OrgData, G, E, <<"OF", _:16, Len:32/little, Data/binary>>) ->
    decode_common({explicit, little}, OrgData, G, E, "OF", Len, Data);
decode_correct_vr_and_length({explicit, big}, OrgData, G, E, <<"OF", _:16, Len:32/big, Data/binary>>) ->
    decode_common({explicit, big}, OrgData, G, E, "OF", Len, Data);

decode_correct_vr_and_length({explicit, little}, OrgData, G, E, <<VRTag:16/bitstring, Len:16/little, Data/binary>>) ->
    VR = binary_to_list(VRTag),
    decode_common({explicit, little}, OrgData, G, E, VR, Len, Data);
decode_correct_vr_and_length({explicit, big}, OrgData, G, E, <<VRTag:16/bitstring, Len:16/big, Data/binary>>) ->
    VR = binary_to_list(VRTag),
    decode_common({explicit, big}, OrgData, G, E, VR, Len, Data);

decode_correct_vr_and_length(_Strategy, OrgData, G, E, _Data) ->
    {error, OrgData, ["unsupported vr", G, E]}.

%%-------------------------------------------------------------------
%% @doc Decode Common With Decoder
%%
%% @end
%%-------------------------------------------------------------------
decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, Decoder) ->
    case wolfpacs_utils:split(Data, Len) of
	{error, _, _} ->
	    {error, OrgData, ["unable to split"]};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, Decoder:decode(Strategy, Bytes)}, Rest}
    end.

%%-------------------------------------------------------------------
%% @doc Decode Common
%%
%% @end
%%-------------------------------------------------------------------
decode_common(Strategy, OrgData, G, E, "OB", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_ob);

decode_common(Strategy, OrgData, G, E, "OW", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_ow);

decode_common(Strategy, OrgData, G, E, "OF", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_of);

decode_common(Strategy, OrgData, G, E, "PN", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_pn);

decode_common(Strategy, OrgData, G, E, "LO", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_lo);

decode_common(Strategy, OrgData, G, E, "AE", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_ae);

decode_common(Strategy, OrgData, G, E, "UI", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_ui);

decode_common(Strategy, OrgData, G, E, "US", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_us);

decode_common(Strategy, OrgData, G, E, "UL", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_ul);

decode_common(Strategy, OrgData, G, E, "CS", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_cs);

decode_common(Strategy, OrgData, G, E, "UN", Len, Data) ->
    decode_common_with_decoder(Strategy, OrgData, G, E, Len, Data, wolfpacs_vr_un);

decode_common(_, OrgData, _G, _E, VR, _Len, _Data) ->
    {error, OrgData, ["unsupported vr", VR]}.

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
    Strategy = {explicit, little},
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe", 0>>,
    ?assertEqual(encode(Strategy, 16, 16, "PN", "Smith^Joe"), TestData).

decode_book_explicit_little_example_test() ->
    %% Page 51
    Strategy = {explicit, little},
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe", 0>>,
    Correct = {{16, 16}, <<"Smith^Joe">>},
    ?assertEqual(decode(Strategy, TestData), {ok, Correct, <<>>}).

encode_book_implicit_little_example_test() ->
    %% Page 51
    Strategy = {implicit, little},
    TestData = <<16, 0, 16, 0, 10, 0, 0, 0, "Smith^Joe", 0>>,
    ?assertEqual(encode(Strategy, 16, 16, "PN", "Smith^Joe"), TestData).

decode_book_implicit_little_example_test() ->
    %% Page 51
    Strategy = {implicit, little},
    TestData = <<16, 0, 16, 0, 10, 0, 0, 0, "Smith^Joe", 0>>,
    Correct = {{16, 16}, <<"Smith^Joe">>},
    ?assertEqual(decode(Strategy, TestData), {ok, Correct, <<>>}).

%%                       __  __ _       _                 _
%%                      |  \/  (_)_ __ (_)_ __ ___   __ _| |
%%                      | |\/| | | '_ \| | '_ ` _ \ / _` | |
%%                      | |  | | | | | | | | | | | | (_| | |
%%                      |_|  |_|_|_| |_|_|_| |_| |_|\__,_|_|
%%

encode_missing_vr_test() ->
    ?assertEqual(encode({explicit, little}, 1, 0, "XY", 123), <<"errorXY">>).

minimal_encode_decode_1_test() ->
    Strategy = {explicit, little},
    Data = [1, 2, 3, 4],
    Correct = {{100, 200}, Data},
    Encoded0 = encode(Strategy, 100, 200, "OB", Data),
    ?assertEqual(decode(Strategy, Encoded0), {ok, Correct, <<>>}).

minimal_encode_decode_2_test() ->
    Strategy = {explicit, big},
    Data = [1, 2, 3, 4],
    Correct = {{100, 200}, Data},
    Encoded0 = encode(Strategy, 100, 200, "OB", Data),
    ?assertEqual(decode(Strategy, Encoded0), {ok, Correct, <<>>}).

minimal_encode_decode_3_test() ->
    Strategy = {implicit, little},
    Data = [1, 2, 3, 4],
    {Group, Element} = wolfpacs_group_elements:vr_to_example_group_element("OB"),
    Correct = {{Group, Element}, Data},
    Encoded0 = encode(Strategy, Group, Element, "OB", Data),
    ?assertEqual(decode(Strategy, Encoded0), {ok, Correct, <<>>}).

minimal_encode_decode_4_test() ->
    Strategy = {implicit, big},
    Data = [1, 2, 3, 4],
    {Group, Element} = wolfpacs_group_elements:vr_to_example_group_element("OB"),
    Correct = {{Group, Element}, Data},
    Encoded0 = encode(Strategy, Group, Element, "OB", Data),
    ?assertEqual(decode(Strategy, Encoded0), {ok, Correct, <<>>}).

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

encode_decode_common(VR, Data) ->
    lists:flatten(
      [ encode_decode_common({explicit, little}, VR, Data)
      , encode_decode_common({explicit, big}, VR, Data)
      , encode_decode_common({implicit, little}, VR, Data)
      , encode_decode_common({implicit, big}, VR, Data)
      ]).

encode_decode_common(Strategy, VR, Data) ->
    {G, E} = wolfpacs_group_elements:vr_to_example_group_element(VR),
    Encoded0 = encode(Strategy, G, E, VR, Data),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1>>,

    ErrorMsg0 = ["unable to split"],
    ErrorMsg1 = ["unable to handle strategy"],

    [?_assertEqual(decode(Strategy, Encoded0), {ok, {{G, E}, Data}, <<>>}),
     ?_assertEqual(decode(Strategy, Encoded1), {ok, {{G, E}, Data}, <<42>>}),
     ?_assertEqual(decode(Strategy, Incorrect0), {error, Incorrect0, ErrorMsg0}),
     ?_assertEqual(decode(Strategy, Incorrect1), {error, Incorrect1, ErrorMsg1})].
