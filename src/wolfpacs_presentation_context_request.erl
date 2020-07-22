%%%-------------------------------------------------------------------
%% @doc WolfPACS's DICOM Presentation Context.
%%
%% A presentation Context, during Association-RQ, is composed of one Abstract Syntax and
%% a list of negotiable Transfer Syntax.
%%
%% Abstract Syntax - "What"
%% Transfer Syntax - "How"
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_presentation_context_request).
-export([encode/4,
	 decode/2]).
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").
-include("wolfpacs_types.hrl").

%%-------------------------------------------------------------------
%% @doc Encodes a presentation contex during Associate-RQ.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(flow(), byte(), binary(), list(binary())) -> binary().
encode(_Flow, PrCID, AbstractSyntax, TransferSyntax) ->
    What = wolfpacs_abstract_syntax:encode(AbstractSyntax),
    How = wolfpacs_transfer_syntax:encode_list(TransferSyntax),
    WhatHow = <<What/binary,
		How/binary>>,
    Payload = <<PrCID,
		0,
		0,
		0,
		WhatHow/binary>>,
    Length = byte_size(Payload),
    <<16#20,
      0,
      Length:16,
      Payload/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes a presentation contex during Associate-RQ.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(flow(), binary()) -> {ok, byte(), binary(), list(binary()), binary()} | error.
decode(Flow, <<16#20, _, Length:16, Payload/binary>>) ->
    NbBytes = byte_size(Payload),
    <<PrCID,
      _,
      _,
      _,
      WhatHow/binary>> = Payload,
    case Length =< NbBytes of
	true ->
	    try_decode(Flow, PrCID, WhatHow);
	false ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "not enough data in payload"),
	    error
    end;
decode(Flow, _Payload) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

%%==============================================================================
%% Private
%%==============================================================================

-spec try_decode(flow(), byte(), binary()) -> {ok, byte(), binary(), list(binary()), binary()} | error.
try_decode(Flow, PrCID, Data) ->
    MaybeAbstractSynatx = wolfpacs_abstract_syntax:decode(Data),
    decode_with_abstract_syntax(Flow, PrCID, MaybeAbstractSynatx).

decode_with_abstract_syntax(Flow, PrCID, {ok, AbstractSyntax, Data}) ->
    MaybeTransferSyntax = wolfpacs_transfer_syntax:decode_list(Data),
    decode_with_transfer_syntax(Flow, PrCID, AbstractSyntax, MaybeTransferSyntax);
decode_with_abstract_syntax(Flow, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode abstract syntax"),
    error.

decode_with_transfer_syntax(Flow, PrCID, AbstractSyntax, {ok, TransferSyntax, Rest}) ->
    wolfpacs_flow:good(Flow, ?MODULE, "decoded all parts"),
    {ok, PrCID, AbstractSyntax, TransferSyntax, Rest};
decode_with_transfer_syntax(Flow, _, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode transfer syntax"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    PrCID = 42,
    AbstractSyntax = ?VERIFICATION,
    TransferSyntax = [?IMPLICIT_LITTLE_ENDIAN,
		      ?EXPLICIT_LITTLE_ENDIAN,
		      ?EXPLICIT_BIG_ENDIAN],
    Encoded0 = encode(no_flow, PrCID, AbstractSyntax, TransferSyntax),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1,2,3,4>>,
    [ ?_assertEqual(decode(no_flow, Encoded0), {ok, PrCID, AbstractSyntax, TransferSyntax, <<>>}),
      ?_assertEqual(decode(no_flow, Encoded1), {ok, PrCID, AbstractSyntax, TransferSyntax, <<42>>}),
      ?_assertEqual(decode(no_flow, Incorrect0), error),
      ?_assertEqual(decode(no_flow, Incorrect1), error)].
