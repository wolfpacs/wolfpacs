%%%-------------------------------------------------------------------
%% @doc Associate Accept (AC)
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_associate_ac).
-export([encode/8,
	 decode/1]).

%%-------------------------------------------------------------------
%% @doc Encodes an Associate Accept.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(binary(), binary(), binary(), byte(), binary(), non_neg_integer(), binary(), binary()) -> binary().
encode(CalledAE, CallingAE, R, PrCID, TransferSyntax, MaxPDUSize, Class, VersionName) ->
    VariableItems = wolfpacs_variable_items_accept:encode(PrCID, TransferSyntax, MaxPDUSize, Class, VersionName),
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
decode(_) ->
    <<>>.

%%==============================================================================
%% Private
%%==============================================================================

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
    ?assertEqual(encode(CalledAE, CallingAE, R, PrCID, TransferSyntax, MaxPDUSize, Class, VersionName),
		 Correct).
