%%%-------------------------------------------------------------------
%% @doc Value Representation - Common Helper functions.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_common).
-export([encode/4,
	 encode_exact/5,
	 encode_limit/5,
	 decode/3
	]).

-include("wolfpacs_types.hrl").

%%------------------------------------------------------------------------------
%% @doc Encode
%%
%% @end
%%------------------------------------------------------------------------------
encode(Flow, Module, Data, PadChar) ->
    Bytes = wolfpacs_vr_utils:pad(Data, PadChar),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

encode_exact(Flow, Module, Data, Length, PadChar) ->
    Bytes = wolfpacs_vr_utils:exact(Data, Length, PadChar),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

encode_limit(Flow, Module, Data, Length, PadChar) ->
    Padded = wolfpacs_vr_utils:pad(Data, PadChar),
    Bytes = wolfpacs_vr_utils:limit(Padded, Length),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

%%------------------------------------------------------------------------------
%% @doc Decode Binary
%%
%% @end
%%------------------------------------------------------------------------------
-spec decode(flow(), module(), binary()) -> {ok, binary(), binary()}.
decode(Flow, Module, Data) ->
    Bytes = wolfpacs_vr_utils:trim(Data),
    wolfpacs_flow:consumed(Flow, Module, byte_size(Data)),
    {ok, Bytes, <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Encoded = encode(no_flow, ?MODULE, <<"abc">>, " "),
    ?assertEqual(Encoded, <<"abc ">>).

encode_exact_test_() ->
    Encoded0 = encode_exact(no_flow, ?MODULE, <<"abc">>, 0, " "),
    Encoded1 = encode_exact(no_flow, ?MODULE, <<"abc">>, 2, " "),
    Encoded2 = encode_exact(no_flow, ?MODULE, <<"abc">>, 4, " "),
    Encoded3 = encode_exact(no_flow, ?MODULE, <<"abc">>, 6, " "),
    [ ?_assertEqual(Encoded0, <<"">>)
    , ?_assertEqual(Encoded1, <<"ab">>)
    , ?_assertEqual(Encoded2, <<"abc ">>)
    , ?_assertEqual(Encoded3, <<"abc   ">>)
    ].
