-module(wolfpacs_vr_common).
-export([encode/3,
	 decode/3,
	 encode_with_limit/4
	]).

-import(wolfpacs_vr_utils, [pad_binary/1,
			    limit_binary/2,
			    trim_binary/1]).

%%------------------------------------------------------------------------------
%% @doc Encode
%%
%% @end
%%------------------------------------------------------------------------------
encode(Flow, Module, Data) when is_list(Data) ->
    encode(Flow, Module, list_to_binary(Data));
encode(Flow, Module, Data) ->
    Bytes = pad_binary(Data),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

%%------------------------------------------------------------------------------
%% @doc Decode
%%
%% @end
%%------------------------------------------------------------------------------
decode(Flow, Module, Data) ->
    Bytes = trim_binary(Data),
    wolfpacs_flow:consumed(Flow, Module, byte_size(Bytes)),
    {ok, Bytes, <<>>}.

%%------------------------------------------------------------------------------
%% @doc Encode with limit
%%
%% @end
%%------------------------------------------------------------------------------
encode_with_limit(Flow, Module, Limit, Data) when is_list(Data) ->
    encode_with_limit(Flow, Module, Limit, list_to_binary(Data));
encode_with_limit(Flow, Module, Limit, Data) ->
    Bytes = limit_binary(pad_binary(Data), Limit),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
