%%%-------------------------------------------------------------------
%% @doc Value Representation Time.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_tm).
-export([encode/2,
	 decode/2]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    trim_binary/1]).

-type un() :: list() | binary().

encode(_Strategy, UN) ->
    encode(UN).

decode(_Strategy, UN) ->
    decode(UN).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(un()) -> binary().
encode(UN) when is_list(UN) ->
    encode(list_to_binary(UN));
encode(UN) ->
    pad_binary(UN).

-spec decode(binary()) -> binary().
decode(<<>>) ->
    {error, <<>>, ["empty TM"]};
decode(Data) ->
    {ok, trim_binary(Data), <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
