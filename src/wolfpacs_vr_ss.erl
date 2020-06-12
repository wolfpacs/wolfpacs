%%%-------------------------------------------------------------------
%% @doc Value Representation Signed Short (SS).
%%
%% Signed binary integer 16 bits long in 2's complement form.
%% Represents an integer n in the range: n in [-128, 127]
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ss).
-export([encode/3,  decode/3]).

-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), integer()) -> binary().
encode(Flow, {_, little}, SS) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<SS:16/little-signed>>;
encode(Flow, {_, big}, SS) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<SS:16/big-signed>>.

-spec decode(flow(), strategy(), binary()) -> integer().
decode(Flow, {_, little}, <<SS:16/little-signed>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, 4),
    {ok, SS, <<>>};
decode(Flow, {_, big}, <<SS:16/big-signed>>) ->
    wolfpacs_flow:consumed(Flow, ?MODULE, 4),
    {ok, SS, <<>>};
decode(Flow, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode SS"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode(Flow, Strategy, Value) ->
    Encoded = encode(Flow, Strategy, Value),
    Decoded = decode(Flow, Strategy, Encoded),
    ?_assertEqual(Decoded, {ok, Value, <<>>}).

encode_decode_all_strategies(Flow, Value) ->
    [ encode_decode(Flow, {explicit, little}, Value)
    , encode_decode(Flow, {explicit, big}, Value)
    , encode_decode(Flow, {implicit, little}, Value)
    , encode_decode(Flow, {implicit, big}, Value)
    ].


encode_decode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    lists:flatten(
      [ encode_decode_all_strategies(Flow, 0)
      , encode_decode_all_strategies(Flow, -128)
      , encode_decode_all_strategies(Flow, 127)
      ]
     ).
