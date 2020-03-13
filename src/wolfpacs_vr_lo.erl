%%%-------------------------------------------------------------------
%% @doc Value Representation Long String.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_lo).
-export([encode/2,
	 decode/2]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    limit_binary/2,
			    trim_binary/1]).

-type lo() :: list() | binary().

encode(_Strategy, LO) ->
    encode(LO).

decode(_Strategy, LO) ->
    decode(LO).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(lo()) -> binary().
encode(LO) when is_list(LO) ->
    encode(list_to_binary(LO));
encode(LO) ->
    limit_binary(pad_binary(LO), 64).

-spec decode(binary()) -> binary().
decode(<<>>) ->
    {error, <<>>, ["empty LO"]};
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
    Long = [$A || _ <- lists:seq(1, 128)],
    Trimmed = list_to_binary([$A || _ <- lists:seq(1, 64)]),
    [?_assertEqual(decode(encode("")), {error, <<>>, ["empty LO"]}),
     ?_assertEqual(decode(encode("A")), {ok, <<"A">>, <<>>}),
     ?_assertEqual(decode(encode("AB")), {ok, <<"AB">>, <<>>}),
     ?_assertEqual(decode(encode("ABC")), {ok, <<"ABC">>, <<>>}),
     ?_assertEqual(decode(encode("ABCD")), {ok, <<"ABCD">>, <<>>}),
     ?_assertEqual(decode(encode(Long)), {ok, Trimmed, <<>>}) ].
