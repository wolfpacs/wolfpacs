%%%-------------------------------------------------------------------
%% @doc Value Representation Long String.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_lo).
-export([encode/3,
	 decode/3]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    limit_binary/2,
			    trim_binary/1]).

-include("wolfpacs_types.hrl").

-type lo() :: list() | binary().

-spec encode(flow(), strategy(), lo()) -> binary().
encode(_Flow, _Strategy, LO) ->
    encode(LO).

-spec decode(flow(), strategy(), binary()) -> {ok, lo(), binary()} | error.
decode(_Flow, _Strategy, LO) ->
    decode(LO).

%%==============================================================================
%% Private
%%==============================================================================

encode(LO) when is_list(LO) ->
    encode(list_to_binary(LO));
encode(LO) ->
    limit_binary(pad_binary(LO), 64).

decode(<<>>) ->
    error;
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
    [?_assertEqual(decode(encode("")), error),
     ?_assertEqual(decode(encode("A")), {ok, <<"A">>, <<>>}),
     ?_assertEqual(decode(encode("AB")), {ok, <<"AB">>, <<>>}),
     ?_assertEqual(decode(encode("ABC")), {ok, <<"ABC">>, <<>>}),
     ?_assertEqual(decode(encode("ABCD")), {ok, <<"ABCD">>, <<>>}),
     ?_assertEqual(decode(encode(Long)), {ok, Trimmed, <<>>}) ].
