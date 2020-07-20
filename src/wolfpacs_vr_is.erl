%%%-------------------------------------------------------------------
%% @doc Integer String Value Representation
%%
%% A string of characters representing an Integer in base-10 (decimal),
%% shall contain only the characters 0 - 9, with an optional
%% leading "+" or "-". It may be padded with leading and/or trailing spaces.
%% Embedded spaces are not allowed.
%%
%% The integer, n, represented shall be in the range: [-231, 230]
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_is).
-export([encode/3,
	 decode/3]).

encode(Flow, Strategy, IS) when is_integer(IS) ->
    encode(Flow, Strategy, integer_to_binary(IS));
encode(Flow, _Strategy, IS) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, IS, " ").

decode(Flow, _Strategy, Data) ->
    case wolfpacs_vr_common:decode(Flow, ?MODULE, Data) of
	{ok, ValueString, <<>>} ->
	    try
		Value = binary_to_integer(ValueString),
		{ok, Value, <<>>}
	    catch
		error:badarg -> error
	    end;
	_ ->
	    error
    end.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = -23,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).

encode_decode_error_test() ->
    Data = <<"Banana">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    Result = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(error, Result).
