%%%-------------------------------------------------------------------
%% @doc Transfer Syntax.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_transfer_syntax).
-export([encode/1,
	 decode/1,
	 encode_list/1,
	 decode_list/1]).
-import(wolfpacs_utils, [drop_last_byte/1]).
-include("wolfpacs_types.hrl").
-include("transfer_syntax.hrl").

-spec encode(binary()) -> binary().
encode(TransferSyntaxString) ->
    Length = byte_size(TransferSyntaxString),
    <<16#40,
      0,
      Length:16,
      TransferSyntaxString/binary>>.

-spec decode(binary()) -> decoded().
decode(Payload = <<16#40, _, Length:16, Data/binary>>) ->
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    TransferSyntaxString = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, NbBytes - Length),
	    {ok, TransferSyntaxString, Rest};
	false ->
	    {error, Payload}
    end;
decode(Data) ->
    {error, Data}.

-spec encode_list(list(binary())) -> binary().
encode_list(ListOfTransferSyntax) ->
    encode_list(ListOfTransferSyntax, <<>>).

-spec decode_list(binary()) -> decoded_list().
decode_list(ListOfTransferSyntax) ->
    decode_list(decode(ListOfTransferSyntax), []).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode_list(list(binary()), binary()) -> binary().
encode_list([], Acc) ->
    Acc;
encode_list([H|T], Acc) ->
    Encoded = encode(H),
    encode_list(T, <<Acc/binary,
		     Encoded/binary>>).

-spec decode_list(decoded(), list(binary())) -> decoded_list().
decode_list({error, Data}, []) ->
    {error, Data};
decode_list({error, Rest}, Acc) ->
    {ok, lists:reverse(Acc), Rest};
decode_list({ok, Decoded, Rest}, Acc) ->
    decode_list(decode(Rest), [Decoded|Acc]).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    V0 = ?IMPLICIT_LITTLE_ENDIAN,
    E0 = encode(V0),
    E1 = <<E0/binary, 42>>,
    I0 = drop_last_byte(E0),
    I1 = <<1,2,3,4>>,
    [ ?_assertEqual(decode(E0), {ok, V0, <<>>}),
      ?_assertEqual(decode(E1), {ok, V0, <<42>>}),
      ?_assertEqual(decode(I0), {error, I0}),
      ?_assertEqual(decode(I1), {error, I1})].

test_encode_list_test_() ->
    Offer = [?IMPLICIT_LITTLE_ENDIAN,
	     ?EXPLICIT_LITTLE_ENDIAN,
	     ?EXPLICIT_BIG_ENDIAN],
    Encoded0 = encode_list(Offer),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect = <<1,2,3,4>>,
    [ ?_assert(decode_list(Encoded0) =:= {ok, Offer, <<>>}),
      ?_assert(decode_list(Encoded1) =:= {ok, Offer, <<42>>}),
      ?_assert(decode_list(Incorrect) =:= {error, Incorrect}) ].
