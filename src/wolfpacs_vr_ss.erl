
%%%-------------------------------------------------------------------
%% @doc Value Representation Signed Short (SS).
%%
%% Signed binary integer 16 bits long in 2's complement form.
%% Represents an integer n in the range: n in [-128, 127]
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ss).
-export([encode/3, decode/3]).

-include("wolfpacs_types.hrl").

encode(Flow, Strategy, Values) when is_list(Values) ->
    Parts = [ encode(Flow, Strategy, Value) || Value <- Values ],
    F = fun(Part, Acc) -> <<Acc/binary, Part/binary>> end,
    lists:foldl(F, <<>>, Parts);
encode(Flow, {_, little}, SS) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<SS:16/little-signed>>;
encode(Flow, {_, big}, SS) ->
    wolfpacs_flow:generated(Flow, ?MODULE, 4),
    <<SS:16/big-signed>>.

decode(Flow, Strategy, Data) when byte_size(Data) > 2 ->
    Parts = wolfpacs_utils:chunk(Data, 2),
    F = fun(Part) -> decode(Flow, Strategy, Part) end,
    wolfpacs_utils:flatten_decoded(lists:map(F, Parts));
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

encode_decode_vm_test_() ->
    S = {explicit, little},
    Values = [1, 16, 256, 1024],
    Encoded = encode(no_flow, S, Values),
    Result = decode(no_flow, S, Encoded),
    [ ?_assertEqual(Result, {ok, Values, <<>>})
    ].
