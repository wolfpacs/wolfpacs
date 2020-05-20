%%%-------------------------------------------------------------------
%% @doc Value Representation Short String.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_sh).
-export([encode/2, decode/2]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    limit_binary/2,
			    trim_binary/1]).

encode(_Strategy, SH) ->
    encode(SH).

decode(_Strategy, SH) ->
    decode(SH).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(list() | binary()) -> binary().
encode(SH) when is_list(SH) ->
    encode(list_to_binary(SH));
encode(SH) ->
    limit_binary(pad_binary(SH), 16).

-spec decode(binary()) -> binary().
decode(<<>>) ->
    {error, <<>>, ["empty SH"]};
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
    [?_assertEqual(decode(encode("")), {error, <<>>, ["empty SH"]}),
     ?_assertEqual(decode(encode("A")), {ok, <<"A">>, <<>>}),
     ?_assertEqual(decode(encode("AB")), {ok, <<"AB">>, <<>>}),
     ?_assertEqual(decode(encode("ABC")), {ok, <<"ABC">>, <<>>}),
     ?_assertEqual(decode(encode("ABCD")), {ok, <<"ABCD">>, <<>>}),
     ?_assertEqual(decode(encode(Long)), {ok, Trimmed, <<>>}) ].
