%%%-------------------------------------------------------------------
%% @doc DICOM Presentation Contexts.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_presentation_contexts_accept).
-export([encode/1,
	 decode/1]).
-include("transfer_syntax.hrl").

encode(Contexts) ->
    encode(Contexts, <<>>).

decode(Data) ->
    case decode(Data, []) of
	error ->
	    {error, Data};
	Success ->
	    Success
    end.

%%==============================================================================
%% Private
%%==============================================================================

encode([], Acc) ->
    Acc;
encode([Context|Contexts], Acc) ->
    {PrCID, TransferSyntax} = Context,
    Data = wolfpacs_presentation_context_accept:encode(no_flow, PrCID, TransferSyntax),
    encode(Contexts, <<Acc/binary, Data/binary>>).

decode(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};
decode(Data = <<16#21, _/binary>>, Acc) ->
    case wolfpacs_presentation_context_accept:decode(no_flow, Data) of
	{ok, PrCID, TransferSyntax, Rest} ->
	    Context = {PrCID, TransferSyntax},
	    decode(Rest, [Context|Acc]);
	_ ->
	    error
    end;
decode(_, []) ->
    %% Atleast one presentation context needed
    %% to register the data as "rest"
    error;
decode(Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    PrCID0 = 0,
    PrCID1 = 1,
    PrCID2 = 2,

    TransferSyntax0 = ?IMPLICIT_LITTLE_ENDIAN,
    TransferSyntax1 = ?EXPLICIT_LITTLE_ENDIAN,
    TransferSyntax2 = ?EXPLICIT_BIG_ENDIAN,

    Contexts = [{PrCID0, TransferSyntax0},
		{PrCID1, TransferSyntax1},
		{PrCID2, TransferSyntax2}],

    Encoded0 = encode(Contexts),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,

    [?_assertEqual(decode(Encoded0), {ok, Contexts, <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, Contexts, <<42>>}),
     ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
     ?_assertEqual(decode(Incorrect1), {error, Incorrect1})].
