%%%-------------------------------------------------------------------
%% @doc User Information.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_user_information).
-export([encode/3,
	 decode/1]).

-spec encode(non_neg_integer(), binary(), binary()) -> binary().
encode(MaxPDUSize, Class, VersionName) ->
    A = wolfpacs_max_length:encode(MaxPDUSize),
    B = wolfpacs_implementation_class:encode(Class),
    C = wolfpacs_version_name:encode(VersionName),
    encode(<<A/binary, B/binary, C/binary>>).

-spec decode(binary()) -> {ok, non_neg_integer(), binary(), binary(), binary()} | {error, binary()}.
decode(Data = <<16#50, _, _Length:16, UserInformation/binary>>) ->
    MaybeMaxLength = wolfpacs_max_length:decode(UserInformation),
    case decode_with_max_length(MaybeMaxLength) of
	{error, Error} ->
	    {error, Data, Error};
	Succes ->
	    Succes
    end;
decode(Data = <<H, _/binary>>) ->
    {error, Data, ["incorrect header", H]};
decode(<<>>) ->
    {error, <<>>, ["no data"]}.

%%==============================================================================
%% Private
%%==============================================================================

encode(UserInformation) ->
    Length = byte_size(UserInformation),
    <<16#50,
      0,
      Length:16,
      UserInformation/binary>>.

decode_with_max_length({ok, MaxSize, Rest}) ->
    MaybeImplementationClass = wolfpacs_implementation_class:decode(Rest),
    decode_with_implementation_class(MaxSize, MaybeImplementationClass);
decode_with_max_length(_) ->
    {error, ["error with_max length"]}.

decode_with_implementation_class(MaxSize, {ok, ImplementationClass, Rest}) ->
    MaybeVersionName = wolfpacs_version_name:decode(Rest),
    decode_with_version_name(MaxSize, ImplementationClass, MaybeVersionName);
decode_with_implementation_class(_, _) ->
    {error, ["error with implimentation class"]}.

decode_with_version_name(MaxSize, ImplementationClass, {ok, VersionName, Rest}) ->
    {ok, MaxSize, ImplementationClass, VersionName, Rest};
decode_with_version_name(_, _, _) ->
    {error, ["error with version name"]}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_echoscu_test() ->
    MaxSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"OFFIS_DCMTK_364">>,
    Encoded = encode(MaxSize, Class, VersionName),
    Correct = <<80,0,0,58,

		81,0,0,4,
		0,0,64,0,

		82,0,0,27,
		49,46,50,46,50,55,54,46,48,46,55,50,51,48,48,49,48,46,51,46,48,46,51,46,54,46,52,

		85,0,0,15,
		79,70,70,73,83,95,68,67,77,84,75,95,51,54,52>>,
    ?assertEqual(Encoded, Correct).

encode_decode_test_() ->
    MaxSize = 16384,
    Class = <<"1">>,
    VersionName = <<"A">>,
    Encoded0 = encode(MaxSize, Class, VersionName),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1,2,3,4>>,
    {ok, Incorrect2} = wolfpacs_utils:clear_byte(Encoded0,  7),
    {ok, Incorrect3} = wolfpacs_utils:clear_byte(Encoded0, 12),
    {ok, Incorrect4} = wolfpacs_utils:clear_byte(Encoded0, 17),

    [ ?_assertEqual(decode(Encoded0), {ok, MaxSize, Class, VersionName, <<>>}),
      ?_assertEqual(decode(Encoded1), {ok, MaxSize, Class, VersionName, <<42>>}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0, ["error with version name"]}),
      ?_assertEqual(decode(Incorrect1), {error, Incorrect1, ["incorrect header", 1]}),
      ?_assertEqual(decode(Incorrect2), {error, Incorrect2, ["error with_max length"]}),
      ?_assertEqual(decode(Incorrect3), {error, Incorrect3, ["error with implimentation class"]}),
      ?_assertEqual(decode(Incorrect4), {error, Incorrect4, ["error with version name"]}) ].
