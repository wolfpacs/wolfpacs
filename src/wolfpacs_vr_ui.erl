%%%-------------------------------------------------------------------
%% @doc Value Representation UI.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ui).
-export([encode/3,  decode/3]).

-include("wolfpacs_types.hrl").

-define(LIMIT, 64).
-define(PAD, 0).

-spec encode(flow(), strategy(), binary()) -> binary().
encode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:encode_limit(Flow, ?MODULE, X, ?LIMIT, ?PAD).

-spec decode(flow(), strategy(), binary()) -> {ok, binary(), binary()} | error.
decode(Flow, _Strategy, X) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, X).

%%==============================================================================
%% Private
%%==============================================================================


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Encoded = encode(no_flow, {explict, little}, <<"1.2.3">>),
    ?assertEqual(Encoded, <<"1.2.3", 0>>).
