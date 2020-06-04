-module(wolfpacs_vr_common).
-export([encode/3,
	 decode/3,
	 encode_with_limit/4,
	 encode_binary/3,
	 decode_binary/3,
	 encode_binary_with_limit/4
	]).

-import(wolfpacs_vr_utils, [pad/1,
			    limit/2,
			    trim/1,
			    pad_binary/1,
			    limit_binary/2,
			    trim_binary/1]).

%%------------------------------------------------------------------------------
%% @doc Encode
%%
%% @end
%%------------------------------------------------------------------------------
encode(Flow, Module, Data) when is_binary(Data) ->
    encode(Flow, Module, binary_to_list(Data));
encode(Flow, Module, Data) ->
    Bytes = list_to_binary(pad(Data)),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

%%------------------------------------------------------------------------------
%% @doc Encode Binary
%%
%% @end
%%------------------------------------------------------------------------------
encode_binary(Flow, Module, Data) when is_list(Data) ->
    encode_binary(Flow, Module, list_to_binary(Data));
encode_binary(Flow, Module, Data) ->
    Bytes = pad_binary(Data),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

%%------------------------------------------------------------------------------
%% @doc Decode Binary
%%
%% @end
%%------------------------------------------------------------------------------
decode(Flow, Module, Data) ->
    Bytes = trim(Data),
    wolfpacs_flow:consumed(Flow, Module, byte_size(Data)),
    {ok, Bytes, <<>>}.

%%------------------------------------------------------------------------------
%% @doc Decode Binary
%%
%% @end
%%------------------------------------------------------------------------------
decode_binary(Flow, Module, Data) ->
    Bytes = trim_binary(Data),
    wolfpacs_flow:consumed(Flow, Module, byte_size(Bytes)),
    {ok, Bytes, <<>>}.

%%------------------------------------------------------------------------------
%% @doc Encode with limit
%%
%% @end
%%------------------------------------------------------------------------------
encode_with_limit(Flow, Module, Limit, Data) when is_binary(Data) ->
    encode_with_limit(Flow, Module, Limit, binary_to_list(Data));
encode_with_limit(Flow, Module, Limit, Data) ->
    Bytes = list_to_binary(limit(pad(Data), Limit)),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

%%------------------------------------------------------------------------------
%% @doc Encode with limit
%%
%% @end
%%------------------------------------------------------------------------------
encode_binary_with_limit(Flow, Module, Limit, Data) when is_list(Data) ->
    encode_binary_with_limit(Flow, Module, Limit, list_to_binary(Data));
encode_binary_with_limit(Flow, Module, Limit, Data) ->
    Bytes = limit_binary(pad_binary(Data), Limit),
    wolfpacs_flow:generated(Flow, Module, byte_size(Bytes)),
    Bytes.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
