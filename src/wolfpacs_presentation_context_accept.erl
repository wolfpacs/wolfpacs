%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%
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
-export([encode/3,
	 decode/2]).
-import(wolfpacs_utils, [drop_last_byte/1]).

-include("wolfpacs_types.hrl").

-spec encode(flow(), byte(), binary()) -> binary().
encode(Flow, PrCID, TransferSyntax) ->
    wolfpacs_flow:success(Flow, ?MODULE),
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

-spec decode(flow(), binary()) -> {ok, byte(), binary(), binary()} | error.
decode(Flow, <<16#21, _, Length:16, Payload/binary>>) ->
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
		    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode transfer syntax"),
		    error
	    end;
	false ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "not enough data to decode transfer syntax"),
	    error
    end;
decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

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
    ?assertEqual(decode(no_flow, Data), {ok, PrCID, TransferSyntax , Rest}).

encode_decode_test_() ->
    PrCID = 1,
    TransferSyntax = <<"1.2.840.10008.1.2">>,
    Encoded0 = encode(no_flow, PrCID, TransferSyntax),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4>>,
    [ ?_assertEqual(decode(no_flow, Encoded0), {ok, PrCID, TransferSyntax, <<>>}),
      ?_assertEqual(decode(no_flow, Encoded1), {ok, PrCID, TransferSyntax, <<42>>}),
      ?_assertEqual(decode(no_flow, Incorrect0), error),
      ?_assertEqual(decode(no_flow, Incorrect1), error)
    ].
