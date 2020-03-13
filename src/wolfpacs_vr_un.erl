%%%-------------------------------------------------------------------
%% @doc Value Representation Patient name.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_un).
-export([encode/2,
	 decode/2]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    trim_binary/1]).

-type un() :: list() | binary().

encode(_Strategy, UN) ->
    encode(UN).

decode(_Strategy, UN) ->
    decode(UN).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(un()) -> binary().
encode(UN) when is_list(UN) ->
    encode(list_to_binary(UN));
encode(UN) ->
    pad_binary(UN).

-spec decode(binary()) -> binary().
decode(<<>>) ->
    {error, <<>>, ["empty UN"]};
decode(Data) ->
    {ok, trim_binary(Data), <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [?_assertEqual(encode(""), <<"">>),
     ?_assertEqual(encode("A"), <<"A", 0>>),
     ?_assertEqual(encode("AB"), <<"AB">>),
     ?_assertEqual(encode("ABC"), <<"ABC", 0>>),
     ?_assertEqual(encode("ABCD"), <<"ABCD">>) ].

encode_decode_test_() ->
    Long = list_to_binary([$A || _ <- lists:seq(1, 1024)]),
    [?_assertEqual(decode(encode("")), {error, <<>>, ["empty UN"]}),
     ?_assertEqual(decode(encode("A")), {ok, <<"A">>, <<>>}),
     ?_assertEqual(decode(encode("AB")), {ok, <<"AB">>, <<>>}),
     ?_assertEqual(decode(encode("ABC")), {ok, <<"ABC">>, <<>>}),
     ?_assertEqual(decode(encode("ABCD")), {ok, <<"ABCD">>, <<>>}),
     ?_assertEqual(decode(encode(Long)), {ok, Long, <<>>}) ].
