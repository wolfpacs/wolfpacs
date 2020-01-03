-module(wolfpacs_presentation_contexts_request).
-export([encode/1,
	 decode/1]).

%% TODO: Turn {PrCID, AbstractSyntax, TransferSyntaxes} into a record

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
    {PrCID, AbstractSyntax, TransferSyntax} = Context,
    Data = wolfpacs_presentation_context_request:encode(PrCID, AbstractSyntax, TransferSyntax),
    encode(Contexts, <<Acc/binary, Data/binary>>).

decode(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};
decode(Data = <<16#20, _/binary>>, Acc) ->
    case wolfpacs_presentation_context_request:decode(Data) of
	{ok, PrCID, AbstractSyntax, TransferSyntax, Rest} ->
	    Context = {PrCID, AbstractSyntax, TransferSyntax},
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

    AbstractSyntax = wolfpacs_sop:verification(),

    TransferSyntax0 = [wolfpacs_transfer_syntax:implicit_vr_little_endian()],
    TransferSyntax1 = [wolfpacs_transfer_syntax:explicit_vr_little_endian()],
    TransferSyntax2 = [wolfpacs_transfer_syntax:explicit_vr_big_endian()],

    Contexts = [{PrCID0, AbstractSyntax, TransferSyntax0},
		{PrCID1, AbstractSyntax, TransferSyntax1},
		{PrCID2, AbstractSyntax, TransferSyntax2}],

    Encoded0 = encode(Contexts),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,

    [?_assertEqual(decode(Encoded0), {ok, Contexts, <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, Contexts, <<42>>}),
     ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
     ?_assertEqual(decode(Incorrect1), {error, Incorrect1})].
