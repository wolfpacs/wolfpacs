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
    wolfpacs_vr_common:encode_with_limit(Flow, ?MODULE, 16, AE).

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).
