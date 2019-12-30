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

-module(wolfpacs_presentation_context_accept).
-export([encode/2,
	 decode/1]).
-import(wolfpacs_utils, [drop_last_byte/1]).

-spec encode(byte(), binary()) -> binary().
encode(PrCID, TransferSyntax) ->
    How = wolfpacs_transfer_syntax:encode(TransferSyntax),
    Payload = <<PrCID,
		0,
		0,
		0,
		How/binary>>,
    Length = byte_size(Payload),
    <<16#21,
      0,
      Length:16,
      Payload/binary>>.

-spec decode(binary()) -> {ok, byte(), binary(), binary()} | {error, binary()}.
decode(AllData = <<16#21, _, Length:16, Payload/binary>>) ->
    NbBytes = byte_size(Payload),
    <<PrCID,
      _,
      _,
      _,
      How/binary>> = Payload,
    case Length =< NbBytes of
	true ->
	    case wolfpacs_transfer_syntax:decode(How) of
		{ok, TransferSyntax, Rest} ->
		    {ok, PrCID, TransferSyntax, Rest};
		_ ->
		    {error, AllData}
	    end;
	false ->
	    {error, AllData}
    end;
decode(AllData) ->
    {error, AllData}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

storescp_echoscu_test() ->
    PrCID = 1,
    TransferSyntax = <<"1.2.840.10008.1.2">>,
    Rest = <<>>,
    Data = wolfpacs_utils:log_to_binary(
	     "21, 00, 00, 19, 01, 00, 00, 00,
	     40, 00, 00, 11,
	     31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 31, 2e,
	     32"),
    ?assertEqual(decode(Data), {ok, PrCID, TransferSyntax , Rest}).

encode_decode_test_() ->
    PrCID = 1,
    TransferSyntax = <<"1.2.840.10008.1.2">>,
    Encoded0 = encode(PrCID, TransferSyntax),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4>>,
    [ ?_assertEqual(decode(Encoded0), {ok, PrCID, TransferSyntax, <<>>}),
      ?_assertEqual(decode(Encoded1), {ok, PrCID, TransferSyntax, <<42>>}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
      ?_assertEqual(decode(Incorrect1), {error, Incorrect1}) ].
