%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
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

-spec encode(binary()) -> <<_:32, _:_*8>>.
encode(TransferSyntaxString) ->
    Length = byte_size(TransferSyntaxString),
    <<16#40,
      0,
      Length:16,
      TransferSyntaxString/binary>>.

-spec decode(binary()) -> {ok, binary(), binary()} | error.
decode(<<16#40, _, Length:16, Data/binary>>) ->
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    TransferSyntaxString = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, NbBytes - Length),
	    {ok, TransferSyntaxString, Rest};
	false ->
	    error
    end;
decode(_Data) ->
    error.

-spec encode_list(list(binary())) -> binary().
encode_list(ListOfTransferSyntax) ->
    encode_list(ListOfTransferSyntax, <<>>).

-spec decode_list(binary()) -> {ok, list(binary()), binary()} | error.
decode_list(ListOfTransferSyntax) ->
    decode_list(decode(ListOfTransferSyntax), [], <<>>).

%%==============================================================================
%% Private
%%==============================================================================

encode_list([], Acc) ->
    Acc;
encode_list([H|T], Acc) ->
    Encoded = encode(H),
    encode_list(T, <<Acc/binary,
		     Encoded/binary>>).

decode_list(error, [], _Rest) ->
    error;
decode_list(error, Acc, Rest) ->
    {ok, lists:reverse(Acc), Rest};
decode_list({ok, Decoded, Rest}, Acc, _PrevRest) ->
    decode_list(decode(Rest), [Decoded|Acc], Rest).

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
      ?_assertEqual(decode(I0), error),
      ?_assertEqual(decode(I1), error)
    ].

test_encode_list_test_() ->
    Offer = [?IMPLICIT_LITTLE_ENDIAN,
	     ?EXPLICIT_LITTLE_ENDIAN,
	     ?EXPLICIT_BIG_ENDIAN],
    Encoded0 = encode_list(Offer),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect = <<1,2,3,4>>,
    [ ?_assertEqual(decode_list(Encoded0), {ok, Offer, <<>>}),
      ?_assertEqual(decode_list(Encoded1), {ok, Offer, <<42>>}),
      ?_assertEqual(decode_list(Incorrect), error)
    ].
