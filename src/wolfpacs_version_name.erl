%%%-------------------------------------------------------------------
%% @doc Version Name.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_version_name).
-export([encode/1,
	 decode/1]).
-import(wolfpacs_utils, [split/2]).

-spec encode(binary()) -> binary() | {error, too_short} | {error, too_long}.
encode(VersionName) ->
    Length = byte_size(VersionName),
    encode_with_length(VersionName, Length).

-spec decode(binary()) -> {ok, binary(), binary()} | {error, binary()}.
decode(<<16#55, _, Length:16, Data/binary>>) ->
    %% We are lenient, we will accept the Version Names
    %% even if they are longer than 16.
    split(Data, Length);
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Private
%%==============================================================================

encode_with_length(_VersionName, Length) when Length < 1 ->
    {error, too_short};
encode_with_length(_VersionName, Length) when Length > 16 ->
    {error, too_long};
encode_with_length(VersionName, Length) ->
    <<16#55,
      0,
      Length:16,
      VersionName/binary>>.


%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    Value = <<"OFFIS_DCMTK_364">>,
    Encoded0 = encode(Value),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = <<1,2,3,4>>,
    [ ?_assertEqual(decode(Encoded0), {ok, Value, <<>>}),
      ?_assertEqual(decode(Encoded1), {ok, Value, <<42>>}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0}) ].
