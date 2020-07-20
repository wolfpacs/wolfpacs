%%%-------------------------------------------------------------------
%% @doc Value Representation Age String.
%%
%% A string of characters with one of the following formats --
%% nnnD,
%% nnnW,
%% nnnM,
%% nnnY;
%%
%% where nnn shall contain the number of days for D,
%% weeks for W, months for M, or years for Y.
%% Example: "018M" would represent an age of 18 months.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_as).
-export([encode/3, decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), list() | binary()) -> binary().
encode(_Flow, _Strategy, AE) ->
    encode(AE).

-spec decode(flow(), strategy(), binary()) -> {ok, binary(), binary()} | error.
decode(_Flow, _Strategy, Data) ->
    decode(Data).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(list() | binary()) -> binary().
encode(AS) when is_list(AS) ->
    encode(list_to_binary(AS));
encode(AS) ->
    wolfpacs_vr_utils:limit(AS, 4).

-spec decode(binary()) -> {ok, binary(), binary()} | error.
decode(<<>>) ->
    error;
decode(<<A, B, C, "D", Rest/binary>>) ->
    {ok, <<A, B, C, "D">>, Rest};
decode(<<A, B, C, "W", Rest/binary>>) ->
    {ok, <<A, B, C, "W">>, Rest};
decode(<<A, B, C, "M", Rest/binary>>) ->
    {ok, <<A, B, C, "M">>, Rest};
decode(<<A, B, C, "Y", Rest/binary>>) ->
    {ok, <<A, B, C, "Y">>, Rest};
decode(_Data) ->
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    [ ?_assertEqual(encode("018M"), <<"018M">>)
    , ?_assertEqual(encode(Flow, {explicit, little}, "018M"), <<"018M">>)
    ].

decode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    [ ?_assertEqual(decode(<<>>), error)
    , ?_assertEqual(decode(<<"ABCQ">>), error)
    , ?_assertEqual(decode(<<"123WABC">>), {ok, <<"123W">>, <<"ABC">>})
    , ?_assertEqual(decode(Flow, {explicit, little}, <<"123WABC">>), {ok, <<"123W">>, <<"ABC">>})
    ].

encode_decode_test_() ->
    [ ?_assertEqual(decode(encode(<<"123D">>)), {ok, <<"123D">>, <<>>})
    , ?_assertEqual(decode(encode(<<"123W">>)), {ok, <<"123W">>, <<>>})
    , ?_assertEqual(decode(encode(<<"123M">>)), {ok, <<"123M">>, <<>>})
    , ?_assertEqual(decode(encode(<<"123Y">>)), {ok, <<"123Y">>, <<>>})
    , ?_assertEqual(decode(encode(<<"123Y">>)), {ok, <<"123Y">>, <<>>})
    ].
