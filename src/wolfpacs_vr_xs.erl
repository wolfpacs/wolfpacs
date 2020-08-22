%%%-------------------------------------------------------------------
%% @doc Value Representation XS.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_xs).
-export([encode/3,
	 decode/3]).

encode(Flow, {_, Endian}, {Endian, Blob}) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode XS"),
    Blob;

encode(Flow, {_, little}, Value) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode XS"),
    <<Value:16/little-unsigned>>;

encode(Flow, {_, big}, Value) ->
    wolfpacs_flow:good(Flow, ?MODULE, "encode XS"),
    <<Value:16/big-unsigned>>.

decode(Flow, {_, Endian}, Blob) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decode XS"),
    {ok, {Endian, Blob}, <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Value = <<1:16/little-signed>>,
    Strategy = {explicit, little},
    Encoded0 = encode(no_flow, Strategy, {little, Value}),

    ?assertEqual(decode(no_flow, Strategy, Encoded0), {ok, {little, Value}, <<>>}).
