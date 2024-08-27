%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
%%
%% @doc Associate Accept (AC)
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_associate_ac).
-export([encode/7,
	 decode/1]).

%%-------------------------------------------------------------------
%% @doc Encodes an Associate Accept.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(binary(), binary(), binary(), list({byte(), binary()}), non_neg_integer(), binary(), binary()) -> binary().
encode(CalledAE, CallingAE, R, SupportedContexts, MaxPDUSize, Class, VersionName) ->
    VariableItems = wolfpacs_variable_items_accept:encode(SupportedContexts, MaxPDUSize, Class, VersionName),
    Payload = <<1:16,  %% Protocol Version
		0:16,  %% Reserved
		CalledAE/binary,
		CallingAE/binary,
		R/binary,
		VariableItems/binary>>,
    Length = byte_size(Payload),
    <<16#2,
      0,
      Length:32,
      Payload/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes an Associate Accept.
%%
%% @end
%%-------------------------------------------------------------------
decode(AllData = <<16#2, _, Length:32, Data/binary>>) ->
    case wolfpacs_utils:split(Data, Length) of
	{ok, Part, Rest} ->
	    case decode_info(Part) of
		{ok, CalledAE, CallingAE, R, Contexts, MaxPDUSize, Class, VersionName, _} ->
		    {ok, CalledAE, CallingAE, R, Contexts, MaxPDUSize, Class, VersionName, Rest};
		{error, _, Error} ->
		    {error, AllData, ["unable to decode info"|Error]}
	    end;
	_ ->
	    {error, AllData, ["not enough data"]}
    end;
decode(AllData) ->
    {error, AllData, ["incorrect header"]}.

%%==============================================================================
%% Private
%%==============================================================================

decode_info(AllData = <<_:16, _:16, CalledAE:128/bitstring, CallingAE:128/bitstring, R:256/bitstring, Data/binary>>) ->
    case wolfpacs_variable_items_accept:decode(Data) of
	{ok, _, Contexts, MaxPDUSize, Class, VersionName, _} ->
	    {ok, CalledAE, CallingAE, R, Contexts, MaxPDUSize, Class, VersionName, <<>>};
	{error, _, Error} ->
	    {error, AllData, ["unable to decode items accept"|Error]}
    end;
decode_info(OrgData) ->
    {error, OrgData, ["not enough data for decode_info"]}.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

storescp_echoscu_test() ->
    CalledAE =  <<"ANY-SCP         ">>,
    CallingAE = <<"ECHOSCU         ">>,
    R = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    PrCID = 1,
    TransferSyntax = <<"1.2.840.10008.1.2">>,
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Correct = wolfpacs_utils:log_to_binary(
	"02, 00, 00, 00, 00, b8, 00, 01, 00, 00, 41, 4e, 59, 2d, 53, 43,
         50, 20, 20, 20, 20, 20, 20, 20, 20, 20, 45, 43, 48, 4f, 53, 43,
         55, 20, 20, 20, 20, 20, 20, 20, 20, 20,

         00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
         00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,

         10, 00, 00, 15,
         31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 33, 2e,
         31, 2e, 31, 2e, 31,

         21, 00, 00, 19,
         01, 00, 00, 00,
         40, 00, 00, 11,
         31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 31, 2e,
         32,

         50, 00, 00, 3a, 51, 00, 00, 04, 00, 00, 40, 00,

         52, 00, 00, 1b,
         31, 2e, 32, 2e, 32, 37, 36, 2e, 30, 2e, 37, 32, 33, 30, 30, 31,
         30, 2e, 33, 2e, 30, 2e, 33, 2e, 36, 2e, 34,

         55, 00, 00, 0f,
         4f, 46, 46, 49, 53, 5f, 44, 43, 4d, 54, 4b, 5f, 33, 36, 34"),
    ?assertEqual(encode(CalledAE, CallingAE, R, [{PrCID, TransferSyntax}], MaxPDUSize, Class, VersionName),
		 Correct).

encode_decode_test_() ->
    CalledAE =  <<"ANY-SCP         ">>,
    CallingAE = <<"ECHOSCU         ">>,
    R = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    PrCID = 1,
    TransferSyntax = <<"1.2.840.10008.1.2">>,
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,

    Encoded0 = encode(CalledAE, CallingAE, R,
		      [{PrCID, TransferSyntax}],
		      MaxPDUSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4, 5>>,
    Incorrect3 = binary:replace(Encoded0, <<"1.2.840">>, <<>>),
    Incorrect4 = binary:replace(Encoded0, <<"3.1.1.1">>, <<>>, [global]),
    Incorrect5 = wolfpacs_utils:clear_even(Encoded0),
    Incorrect6 = wolfpacs_utils:clear_odd(Encoded0),
    Incorrect7 = binary:replace(Encoded0, <<16#21>>, <<0>>, [global]),

    Correct0 = {ok, CalledAE, CallingAE, R, [{PrCID, TransferSyntax}], MaxPDUSize, Class, VersionName, <<>>},
    Correct1 = {ok, CalledAE, CallingAE, R, [{PrCID, TransferSyntax}], MaxPDUSize, Class, VersionName, <<42>>},

    [ ?_assertEqual(decode(Encoded0), Correct0)
    , ?_assertEqual(decode(Encoded1), Correct1)
    , ?_assertEqual(decode(Incorrect0), {error, Incorrect0, ["not enough data"]})
    , ?_assertEqual(decode(Incorrect1), {error, Incorrect1, ["incorrect header"]})
    , ?_assertEqual(decode(Incorrect2), {error, Incorrect2, ["incorrect header"]})
    , ?_assertEqual(decode(Incorrect3), {error, Incorrect3, ["not enough data"]})
    , ?_assertEqual(decode(Incorrect4), {error, Incorrect4, ["not enough data"]})
    , ?_assertEqual(decode(Incorrect5), {error, Incorrect5, ["incorrect header"]})
    , ?_assertEqual(decode(Incorrect6), {error, Incorrect6, ["unable to decode info",
							     "not enough data for decode_info"]})
    , ?_assertEqual(decode(Incorrect7), {error, Incorrect7, ["unable to decode info",
							     "unable to decode items accept",
							     "unable to decode accept"]})
    ].
