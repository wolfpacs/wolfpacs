%%%-------------------------------------------------------------------
%% @doc Data Elements (Explicit Little).
%%
%% @ref http://dicom.nema.org/medical/dicom/2014c/output/chtml/part05/chapter_7.html#sect_7.1.1
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_element_implicit).
-export([encode/3,
	 encode/4,
	 decode/1]).

encode(G, E, Data) ->
    encode_vr(G, E, Data, wolfpacs_group_elements:vr(G, E)).

encode(G, E, VR, Data) ->
    encode_vr(G, E, Data, VR).

decode(OrgData = <<G:16/little, E:16/little, Length:32/little, Data/binary>>) ->
    case wolfpacs_utils:split(Data, Length) of
	{error, _} ->
	    {error, OrgData};
	{ok, Element, Rest} ->
	    case decode_vr(wolfpacs_group_elements:vr(G, E), Element) of
		{ok, Value} ->
		    {ok, {{G, E}, Value}, Rest};
		_ ->
		    {error, OrgData}
	    end
    end;
decode(OrgData) ->
    {error, OrgData}.

%%==============================================================================
%% Private Encoders
%%==============================================================================

encode_vr(G, E, Data, "OB") ->
    encode_common(G, E, wolfpacs_vr_ob:encode(Data));
encode_vr(G, E, Data, "OW") ->
    encode_common(G, E, wolfpacs_vr_ow:encode_little(Data));
encode_vr(G, E, Data, "OF") ->
    encode_common(G, E, wolfpacs_vr_of:encode_little(Data));
encode_vr(G, E, Data, "AE") ->
    encode_common(G, E, wolfpacs_vr_ae:encode(Data));
encode_vr(G, E, Data, "UI") ->
    encode_common(G, E, wolfpacs_vr_ui:encode(Data));
encode_vr(G, E, Data, "US") ->
    encode_common(G, E, wolfpacs_vr_us:encode_little(Data));
encode_vr(G, E, Data, "UL") ->
    encode_common(G, E, wolfpacs_vr_ul:encode_little(Data));
encode_vr(G, E, Data, "PN") ->
    encode_common(G, E, wolfpacs_vr_pn:encode(Data));
encode_vr(G, E, Data, VR) ->
    lager:warning("[data_element_implicit] Unable to encode ~p", [VR]),
    encode_common(G, E, Data).

encode_common(G, E, Bytes) ->
    NbBytes = byte_size(Bytes),
    <<G:16/little, E:16/little, NbBytes:32/little, Bytes/binary>>.

%%==============================================================================
%% Private Decoders
%%==============================================================================

decode_vr("OB", Element) ->
    {ok, wolfpacs_vr_ob:decode(Element)};
decode_vr("OW", Element) ->
    {ok, wolfpacs_vr_ow:decode_little(Element)};
decode_vr("OF", Element) ->
    {ok, wolfpacs_vr_of:decode_little(Element)};
decode_vr("AE", Element) ->
    {ok, wolfpacs_vr_ae:decode(Element)};
decode_vr("UI", Element) ->
    {ok, wolfpacs_vr_ui:decode(Element)};
decode_vr("PN", Element) ->
    {ok, wolfpacs_vr_pn:decode(Element)};
decode_vr("US", Element) ->
    {ok, wolfpacs_vr_us:decode_little(Element)};
decode_vr("UL", Element) ->
    {ok, wolfpacs_vr_ul:decode_little(Element)};
decode_vr(VR, _) ->
    lager:warning("[data_element_implicit] Unable to decode ~p", [VR]),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_common(G, E, Data) ->
    Encoded0 = encode(G, E, Data),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = <<1>>,
    [ ?_assertEqual(decode(Encoded0), {ok, {{G, E}, Data}, <<>>})
    , ?_assertEqual(decode(Encoded1), {ok, {{G, E}, Data}, <<42>>})
    , ?_assertEqual(decode(Incorrect0), {error, Incorrect0})
    , ?_assertEqual(decode(Incorrect1), {error, Incorrect1})
    , ?_assertEqual(decode(Incorrect2), {error, Incorrect2})
    ].

encode_decode_ob_test_() ->
    encode_decode_common(16#0002, 16#0001, [1, 2, 3, 4, 5]).

encode_decode_ow_test_() ->
    encode_decode_common(16#0028, 16#1201, [1, 2, 3, 4, 5]).

encode_decode_of_test_() ->
    encode_decode_common(16#0064, 16#0009, [1, 2, 3, 4, 5]).

encode_decode_pn_test_() ->
    encode_decode_common(16#0070, 16#0084, <<"Smith^Joe">>).

encode_decode_ae_test_() ->
    encode_decode_common(16#0072, 16#005E, <<"AE1">>).

encode_decode_ui_test_() ->
    encode_decode_common(16#0072, 16#007F, <<"1.2.3">>).

encode_decode_us_test_() ->
    encode_decode_common(16#0072, 16#0100, 1024).

encode_decode_ul_test_() ->
    encode_decode_common(16#0074, 16#1054, 1024).
