%%%-------------------------------------------------------------------
%% @doc WolfPACS's DICOM Presentation Context
%%
%% A presentation Context is composed of one Abstract Syntax and
%% a list of negotiable Transfer Syntax.
%%
%% Abstract Syntax - "What"
%% Transfer Syntax - "How"
%%
%% Request ~> {AbstractSyntax, [TransferSyntax]}.
%%
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_presentation_context).
-include_lib("eunit/include/eunit.hrl").
-export([encode_request/3,
	 decode_request/1]).
-import(wolfpacs_utils, [drop_last_byte/1]).

-spec encode_request(byte(), binary(), list(binary())) -> binary().
encode_request(PrCID, AbstractSyntax, TransferSyntax) ->
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

-spec decode_request(binary()) -> {ok, byte(), binary(), list(binary()), binary()} | {error, binary()}.
decode_request(AllData = <<16#20, _, Length:16, Payload/binary>>) ->
    NbBytes = byte_size(Payload),
    <<PrCID,
      _,
      _,
      _,
      WhatHow/binary>> = Payload,
    case Length =< NbBytes of
	true ->
	    case try_decode_request(PrCID, WhatHow) of
		{error, Type} ->
		    lager:warning("unable to decode ~p", [Type]),
		    {error, AllData};
		Success ->
		    Success
	    end;
	false ->
	    {error, AllData}
    end;
decode_request(Payload) ->
    {error, Payload}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

-spec try_decode_request(byte(), binary()) -> {ok, byte(), binary(), list(binary()), binary()} | {error, atom()}.
try_decode_request(PrCID, Data) ->
    MaybeAbstractSynatx = wolfpacs_abstract_syntax:decode(Data),
    decode_request_with_abstract_syntax(PrCID, MaybeAbstractSynatx).

decode_request_with_abstract_syntax(PrCID, {ok, AbstractSyntax, Data}) ->
    MaybeTransferSyntax = wolfpacs_transfer_syntax:decode_list(Data),
    decode_request_with_transfer_syntax(PrCID, AbstractSyntax, MaybeTransferSyntax);
decode_request_with_abstract_syntax(_, _) ->
    {error, abstract_syntax}.

decode_request_with_transfer_syntax(PrCID, AbstractSyntax, {ok, TransferSyntax, Rest}) ->
    {ok, PrCID, AbstractSyntax, TransferSyntax, Rest};
decode_request_with_transfer_syntax(_, _, _) ->
    {error, transfer_syntax}.

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

test_encode_test_() ->
    PrCID = 42,
    AbstractSyntax = wolfpacs_abstract_syntax:verification(),
    TransferSyntax = [wolfpacs_transfer_syntax:implicit_vr_little_endian(),
		      wolfpacs_transfer_syntax:explicit_vr_little_endian(),
		      wolfpacs_transfer_syntax:explicit_vr_big_endian()],
    Encoded = encode_request(PrCID, AbstractSyntax, TransferSyntax),
    Incorrect0 = drop_last_byte(Encoded),
    [ ?_assertEqual(decode_request(Encoded), {ok, PrCID, AbstractSyntax, TransferSyntax, <<>>}),
      ?_assertEqual(decode_request(Incorrect0), {error, Incorrect0}) ].
