%%%-------------------------------------------------------------------
%% @doc Value Representation Application Entity.
%%
%% A string of characters that identifies an Application Entity with
%% leading and trailing spaces (20H) being non-significant.
%% A value consisting solely of spaces shall not be used.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ae).
-export([encode/2, decode/2]).
-include("wolfpacs_types.hrl").
-import(wolfpacs_vr_utils, [pad_binary/1,
			    limit_binary/2,
			    trim_binary/1]).

-spec encode(strategy(), binary()) -> binary().
encode(_Strategy, AE) ->
    encode(AE).

-spec decode(strategy(), binary()) -> {ok, binary(), binary()} | {error, binary(), list(string())}.
decode(_Strategy, Data) ->
    decode(Data).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(list() | binary()) -> binary().
encode(UI) when is_list(UI) ->
    encode(list_to_binary(UI));
encode(UI) ->
    limit_binary(pad_binary(UI), 16).

-spec decode(binary()) -> binary().
decode(<<>>) ->
    {error, <<>>, ["empty AE"]};
decode(Data) ->
    {ok, trim_binary(Data), <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [?_assertEqual(encode(""),     <<"">>),
     ?_assertEqual(encode("A"),    <<"A", 0>>),
     ?_assertEqual(encode("AB"),   <<"AB">>),
     ?_assertEqual(encode("12345678901234567890"),   <<"1234567890123456">>) ].

encode_decode_test_() ->
    Long = [$A || _ <- lists:seq(1, 32)],
    Trimmed = list_to_binary([$A || _ <- lists:seq(1, 16)]),
    [?_assertEqual(decode(encode("")), {error, <<>>, ["empty AE"]}),
     ?_assertEqual(decode(encode("A")), {ok, <<"A">>, <<>>}),
     ?_assertEqual(decode(encode("AB")), {ok, <<"AB">>, <<>>}),
     ?_assertEqual(decode(encode("ABC")), {ok, <<"ABC">>, <<>>}),
     ?_assertEqual(decode(encode("ABCD")), {ok, <<"ABCD">>, <<>>}),
     ?_assertEqual(decode(encode(Long)), {ok, Trimmed, <<>>}) ].
