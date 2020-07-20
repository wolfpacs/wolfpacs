%%%-------------------------------------------------------------------
%% @doc Value Representation Application Entity.
%%
%% A string of characters that identifies an Application Entity with
%% leading and trailing spaces (20H) being non-significant.
%% A value consisting solely of spaces shall not be used.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ae).
-export([encode/3, decode/3]).

encode(Flow, _Strategy, AE) ->
    wolfpacs_vr_common:encode_exact(Flow, ?MODULE, AE, 16, " ").

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = <<"WolfPACS">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).
