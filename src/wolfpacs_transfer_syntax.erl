-module(wolfpacs_transfer_syntax).
-include_lib("eunit/include/eunit.hrl").
-export([implicit_vr_little_endian/0,
	 explicit_vr_little_endian/0,
	 explicit_vr_big_endian/0,
	 encode/1,
	 decode/1]).

implicit_vr_little_endian() ->
    <<"1.2.840.10008.1.2">>.

explicit_vr_little_endian() ->
    <<"1.2.840.10008.1.2.1">>.

explicit_vr_big_endian() ->
    <<"1.2.840.10008.1.2.2">>.

-spec encode(binary()) -> binary().
encode(TransferSyntaxString) ->
    Length = byte_size(TransferSyntaxString),
    <<16#40,
      0,
      Length:16,
      TransferSyntaxString/binary>>.

-spec decode(binary()) -> {ok, binary(), binary()} | {error, binary()}.
decode(<<16#40, _, Length:16, Data/binary>>) ->
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    TransferSyntaxString = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, NbBytes - Length),
	    {ok, TransferSyntaxString, Rest};
	false ->
	    {error, Data}
    end;
decode(Data) ->
    {error, Data}.

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

test_encode_test_() ->
    V0 = implicit_vr_little_endian(),
    E0 = encode(V0),
    E1 = <<E0/binary, 42>>,
    <<_, I0/binary>> = E0,
    I1 = <<1,2,3,4>>,
    [ ?_assert(decode(E0) =:= {ok, V0, <<>>}),
      ?_assert(decode(E1) =:= {ok, V0, <<42>>}),
      ?_assert(decode(I0) =:= {error, I0}),
      ?_assert(decode(I1) =:= {error, I1})].
