%%%-------------------------------------------------------------------
%% @doc Value Representation Short Text.
%%
%% Leading spaces are considered to be significant.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_st).
-export([encode/2,  decode/2]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    limit_binary/2,
			    trim_binary/1]).

encode(_Strategy, AE) ->
    encode(AE).

decode(_Strategy, AE) ->
    decode(AE).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(list() | binary()) -> binary().
encode(UI) when is_list(UI) ->
    encode(list_to_binary(UI));
encode(UI) ->
    limit_binary(pad_binary(UI), 1024).

-spec decode(binary()) -> binary().
decode(<<>>) ->
    {error, <<>>, ["empty ST"]};
decode(Data) ->
    {ok, trim_binary(Data), <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [?_assertEqual(encode(""),     <<"">>),
     ?_assertEqual(encode("A"),    <<"A", 0>>),
     ?_assertEqual(encode("AB"),   <<"AB">>)].

encode_decode_test_() ->
    Long = [$A || _ <- lists:seq(1, 4096)],
    Trimmed = list_to_binary([$A || _ <- lists:seq(1, 1024)]),
    [?_assertEqual(decode(encode("")), {error, <<>>, ["empty ST"]}),
     ?_assertEqual(decode(encode("A")), {ok, <<"A">>, <<>>}),
     ?_assertEqual(decode(encode("AB")), {ok, <<"AB">>, <<>>}),
     ?_assertEqual(decode(encode("ABC")), {ok, <<"ABC">>, <<>>}),
     ?_assertEqual(decode(encode("ABCD")), {ok, <<"ABCD">>, <<>>}),
     ?_assertEqual(decode(encode(Long)), {ok, Trimmed, <<>>}) ].
