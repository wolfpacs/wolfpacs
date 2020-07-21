%%%-------------------------------------------------------------------
%% @doc File Format.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_file_format).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").
-include("transfer_syntax.hrl").

-define(ERROR_DECODE, "unable to decode data element").
-define(ERROR_META, "unable to decode meta file information").

%%-------------------------------------------------------------------
%% @doc Encodes a File Format.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(pid(), strategy(), binary()) -> binary().
encode(Flow, Strategy, Data) ->
    MetaInformation = wolfpacs_file_meta_information:encode(Flow, Strategy, meta_info()),
    <<MetaInformation/binary, Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes a File Format.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(pid(), strategy(), binary()) -> {ok, {map(), map()}, binary()} | error.
decode(Flow, Strategy, Data) ->
    case wolfpacs_file_meta_information:decode(Flow, Strategy, Data) of
	{ok, Meta, Rest} ->
	    TransferSyntax = maps:get({2, 16#10}, Meta, missing),
	    decode_with_transfer_syntax(Flow, Meta, TransferSyntax, Rest);
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, ?ERROR_META),
	    error
    end.

%%==============================================================================
%% Private
%%==============================================================================

decode_with_transfer_syntax(Flow, _Meta, missing, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "transfer syntax missing"),
    error;

decode_with_transfer_syntax(Flow, Meta, ?IMPLICIT_LITTLE_ENDIAN, Data) ->
    Strategy = {implicit, little},
    case wolfpacs_data_elements:decode(Flow, Strategy, Data) of
	{ok, Content, Rest} ->
	    {ok, {Meta, Content}, Rest};
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, ?ERROR_DECODE),
	    error
    end;

decode_with_transfer_syntax(Flow, Meta, ?EXPLICIT_LITTLE_ENDIAN, Data) ->
    Strategy = {explicit, little},
    case wolfpacs_data_elements:decode(Flow, Strategy, Data) of
	{ok, Content, Rest} ->
	    {ok, {Meta, Content}, Rest};
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, ?ERROR_DECODE),
	    error
    end;

decode_with_transfer_syntax(Flow, Meta, ?EXPLICIT_BIG_ENDIAN, Data) ->
    Strategy = {explicit, big},
    case wolfpacs_data_elements:decode(Flow, Strategy, Data) of
	{ok, Content, Rest} ->
	    {ok, {Meta, Content}, Rest};
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, ?ERROR_DECODE),
	    error
    end;

decode_with_transfer_syntax(Flow, _Meta, TransferSyntax, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, io_lib:format("unknown transfer syntax ~p", [TransferSyntax])),
    error.

-spec meta_info() -> map().
meta_info() ->
    #{{2, 1} => [0, 1],
      {2, 2} => <<"1.2.840.10008.5.1.4.1.1.2">>,
      {2, 3} => <<"2.16.840.1.113662.2.1.4519.41582.4105152.419990505.410523251">>,
      {2, 16#10} => <<"1.2.840.10008.1.2.1">>,
      {2, 16#12} => <<"2.16.840.1.113662.2.1.1">>,
      {2, 16#16} => <<"PHOENIXSCP">>
     }.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),

    Strategy = {explicit, little},
    Content = #{{16#7fe0, 16#10, "OB"} => [255, 254, 255, 254]},
    Content2 = #{{16#7fe0, 16#10} => [255, 254, 255, 254]},
    Data = wolfpacs_data_elements:encode(Flow, {explicit, little}, Content),

    Encoded0 = encode(Flow, Strategy, Data),
    Incorrect0 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4, 5>>,

    [ ?_assertEqual(decode(Flow, Strategy, Encoded0), {ok, {meta_info(), Content2}, <<>>})
    , ?_assertEqual(decode(Flow, Strategy, Incorrect0), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect1), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect2), error)
    ].
