%%%-------------------------------------------------------------------
%% @doc Integer String Value Representation
%%
%% A string of characters representing an Integer in base-10 (decimal),
%% shall contain only the characters 0 - 9, with an optional
%% leading "+" or "-". It may be padded with leading and/or trailing spaces.
%% Embedded spaces are not allowed.
%%
%% The integer, n, represented shall be in the range:
%%
%%  -231<= n <= (231-1).
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_is).
-export([encode/3,
	 decode/3]).

encode(Flow, _Strategy, IS) ->
    wolfpacs_vr_common:encode_binary(Flow, ?MODULE, integer_to_list(IS)).

decode(Flow, _Strategy, Data) ->
    case wolfpacs_vr_common:decode(Flow, ?MODULE, Data) of
	{ok, Value, <<>>} ->
	    {ok, list_to_integer(Value), <<>>};
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
