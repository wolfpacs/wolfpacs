%%%-------------------------------------------------------------------
%% @doc Value Representation Time.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_tm).
-export([encode/3,
	 decode/3]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    trim_binary/1]).

encode(Flow, _Strategy, UN) ->
    priv_encode(Flow, UN).

decode(Flow, _Strategy, UN) ->
    priv_decode(Flow, UN).

%%==============================================================================
%% Private
%%==============================================================================

priv_encode(Flow, UN) when is_list(UN) ->
    priv_encode(Flow, list_to_binary(UN));
priv_encode(_Flow, UN) ->
    pad_binary(UN).

priv_decode(Flow, <<>>) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "empty TM"),
    error;
priv_decode(_Flow, Data) ->
    {ok, trim_binary(Data), <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
