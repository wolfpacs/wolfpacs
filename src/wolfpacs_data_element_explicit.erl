%%%-------------------------------------------------------------------
%% @doc Data Elements (Explicit Little).
%%
%% @ref http://dicom.nema.org/medical/dicom/2014c/output/chtml/part05/chapter_7.html#sect_7.1.1
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_element_explicit).
-export([encode/4,
	 decode/1]).

-spec encode(integer(), integer(), list(), any()) -> binary().
encode(G, E, "OB", Bytes) ->
    encode_common_long(G, E, "OB", wolfpacs_vr_ob:encode(Bytes));
encode(G, E, "OW", Bytes) ->
    encode_common_long(G, E, "OW", wolfpacs_vr_ow:encode_little(Bytes));
encode(G, E, "OF", Bytes) ->
    encode_common_long(G, E, "OF", wolfpacs_vr_of:encode_little(Bytes));
encode(G, E, "AE", Title) ->
    encode_common_short(G, E, "AE", wolfpacs_vr_ae:encode(Title));
encode(G, E, "UI", Title) ->
    encode_common_short(G, E, "UI", wolfpacs_vr_ui:encode(Title));
encode(G, E, "US", US) ->
    encode_common_short(G, E, "US", wolfpacs_vr_us:encode_little(US));
encode(G, E, "PN", Name) ->
    encode_common_short(G, E, "PN", wolfpacs_vr_pn:encode(Name));
encode(_, _, VRTag, _) ->
    lager:warning("[data_element_explicit] unable to encode ~p", [VRTag]),
    VR = list_to_binary(VRTag),
    <<"error", VR/binary>>.

-spec decode(binary()) -> {ok, {{integer(), integer()}, any()}, binary()} | {error, binary()}.
decode(OrgData = <<G:16/little, E:16/little, "OB", _:16, Len:32/little, Data/bitstring>>) ->
    case wolfpacs_utils:split(Data, Len) of
	{error, _ } ->
	    {error, OrgData};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, wolfpacs_vr_ob:decode(Bytes)}, Rest}
    end;
decode(OrgData = <<G:16/little, E:16/little, "OW", _:16, Len:32/little, Data/bitstring>>) ->
    case wolfpacs_utils:split(Data, Len) of
	{error, _ } ->
	    {error, OrgData};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, wolfpacs_vr_ow:decode_little(Bytes)}, Rest}
    end;
decode(OrgData = <<G:16/little, E:16/little, "OF", _:16, Len:32/little, Data/bitstring>>) ->
    case wolfpacs_utils:split(Data, Len) of
	{error, _ } ->
	    {error, OrgData};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, wolfpacs_vr_of:decode_little(Bytes)}, Rest}
    end;
decode(OrgData = <<G:16/little, E:16/little, "PN", Len:16/little, Data/bitstring>>) ->
    case wolfpacs_utils:split(Data, Len) of
	{error, _ } ->
	    {error, OrgData};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, wolfpacs_vr_ob:decode(Bytes)}, Rest}
    end;
decode(OrgData = <<G:16/little, E:16/little, "UI", Len:16/little, Data/bitstring>>) ->
    lager:debug("[data_element_explicit] Patient Name"),
    case wolfpacs_utils:split(Data, Len) of
	{error, _ } ->
	    {error, OrgData};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, wolfpacs_vr_ui:decode(Bytes)}, Rest}
    end;
decode(OrgData = <<G:16/little, E:16/little, "US", Len:16/little, Data/bitstring>>) ->
    case wolfpacs_utils:split(Data, Len) of
	{error, _ } ->
	    {error, OrgData};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, wolfpacs_vr_us:decode_little(Bytes)}, Rest}
    end;
decode(OrgData = <<G:16/little, E:16/little, VR:16/bitstring, Len:16/little, Data/bitstring>>) ->
    lager:debug("[data_element_explicit] generic vr ~p", [VR]),
    case wolfpacs_utils:split(Data, Len) of
	{error, _ } ->
	    {error, OrgData};
	{ok, Bytes, Rest} ->
	    {ok, {{G, E}, Bytes}, Rest}
    end;
decode(OrgData) ->
    {error, OrgData}.

%%==============================================================================
%% Private Encoders
%%==============================================================================

-spec encode_common_long(integer(), integer(), list(), binary()) -> binary().
encode_common_long(G, E, VRTag, Data) ->
    Len = byte_size(Data),
    VR = list_to_binary(VRTag),
    <<G:16/little, E:16/little, VR/binary, 0:16, Len:32/little, Data/binary>>.

-spec encode_common_short(integer(), integer(), list(), binary()) -> binary().
encode_common_short(G, E, VRTag, Data) ->
    Len = byte_size(Data),
    VR = list_to_binary(VRTag),
    <<G:16/little, E:16/little, VR/binary, Len:16/little, Data/binary>>.

%%==============================================================================
%% Private Decoders
%%==============================================================================


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_commont_test() ->
    ?assertEqual(encode_common_short(1, 2, "OB", <<1, 2, 3>>),
		 <<1:16/little, 2:16/little, "OB", 3:16/little, 1, 2, 3>>).

encode_book_example_test() ->
    %% Page 51
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe ">>,
    ?assertEqual(encode(16, 16, "PN", "Smith^Joe"), TestData).

decode_book_example_test() ->
    %% Page 51
    TestData = <<16, 0, 16, 0, "PN", 10, 0, "Smith^Joe ">>,
    Correct = {{16, 16}, "Smith^Joe "},
    ?assertEqual(decode(TestData), {ok, Correct, <<>>}).

encode_decode_basic_test() ->
    Data = [1, 2, 3, 4],
    Correct = {{100, 200}, Data},
    Encoded0 = encode(100, 200, "OB", Data),
    ?assertEqual(decode(Encoded0), {ok, Correct, <<>>}).
