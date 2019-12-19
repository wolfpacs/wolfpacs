-module(wolfpacs_abstract_syntax).
-include_lib("eunit/include/eunit.hrl").
-export([verification/0,
	 encode/1,
	 decode/1]).

-spec verification() -> binary().
verification() ->
    <<"1.2.840.10008.1.1">>.

-spec encode(binary()) -> binary().
encode(AbstractSyntaxString) ->
    Length = byte_size(AbstractSyntaxString),
    <<16#30,
      0,
      Length:16,
      AbstractSyntaxString/binary>>.

-spec decode(binary()) -> {ok, binary(), binary()} | {error, binary()}.
decode(<<16#30, _, Length:16, Data/binary>>) ->
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    AbstractSyntaxString = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, NbBytes - Length),
	    {ok, AbstractSyntaxString, Rest};
	false ->
	    {error, Data}
    end;
decode(Data) ->
    {error, Data}.

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

test_encode_test_() ->
    V0 = verification(),
    E0 = encode(V0),
    E1 = <<E0/binary, 42>>,
    [ ?_assert(decode(E0) =:= {ok, V0, <<>>}),
      ?_assert(decode(E1) =:= {ok, V0, <<42>>}) ].
