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
%% @doc DICOM Presentation Contexts.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_presentation_contexts_request).
-export([encode/1,
	 decode/1]).
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").

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
    Data = wolfpacs_presentation_context_request:encode(no_flow, PrCID, AbstractSyntax, TransferSyntax),
    encode(Contexts, <<Acc/binary, Data/binary>>).

decode(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};
decode(Data = <<16#20, _/binary>>, Acc) ->
    case wolfpacs_presentation_context_request:decode(no_flow, Data) of
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

    AbstractSyntax = ?VERIFICATION,

    TransferSyntax0 = [?IMPLICIT_LITTLE_ENDIAN],
    TransferSyntax1 = [?EXPLICIT_LITTLE_ENDIAN],
    TransferSyntax2 = [?EXPLICIT_BIG_ENDIAN],

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
