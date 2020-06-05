%%%-------------------------------------------------------------------
%% @doc Value Representation Attribute Tag.
%%
%% Ordered pair of 16-bit unsigned integers that is the value of a Data Element Tag.
%% Example -
%% A Data Element Tag of (0018,00FF) would be encoded as a series of 4 bytes in a
%% Little-Endian Transfer Syntax as
%%   18H,00H,FFH,00H
%% and in a Big-Endian Transfer Syntax as
%%   00H,18H,00H,FFH.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_at).
-export([encode/3,
	 decode/3]).

encode(_Flow, {_, little}, {First, Second}) ->
    <<First:16/little, Second:16/little>>;
encode(_Flow, {_, big}, {First, Second}) ->
    <<First:16/big, Second:16/big>>.

decode(_Flow, {_, little}, <<First:16/little, Second:16/little>>) ->
    {ok, {First, Second}, <<>>};
decode(_Flow, {_, big}, <<First:16/big, Second:16/big>>) ->
    {ok, {First, Second}, <<>>};
decode(_Flow, _, OrgData) ->
    {error, OrgData, ["unable to decode"]}.

%%==============================================================================
%% Private
%%==============================================================================

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_little_test_() ->
    %% Example taken from standard,
    %% Table A-2: DICOM Value Representations (VR) Types
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded = encode(Flow, {explicit, little}, {16#18, 16#ff}),
    {ok, Decoded, <<>>} = decode(Flow, {explicit, little}, Encoded),
    [ ?_assertEqual(Encoded, <<16#18, 16#00, 16#ff, 16#00>>)
    , ?_assertEqual(Decoded, {16#18, 16#ff})
    ].

encode_decode_big_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    %% Example taken from standard,
    %% Table A-2: DICOM Value Representations (VR) Types
    Encoded = encode(Flow, {explicit, big}, {16#18, 16#ff}),
    {ok, Decoded, <<>>} = decode(Flow, {explicit, big}, Encoded),
    [ ?_assertEqual(Encoded, <<16#00, 16#18, 16#00, 16#ff>>)
    , ?_assertEqual(Decoded, {16#18, 16#ff})
    ].
