%%%-------------------------------------------------------------------
%% @doc Abstract Syntax.
%%
%% Abstract Syntax concerns what information is exchanged.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_abstract_syntax).
-export([encode/1,
	 decode/1]).
-import(wolfpacs_utils, [drop_last_byte/1]).

%%-------------------------------------------------------------------
%% @doc Encodes an Abstract Syntax UID.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(AbstractSyntaxUID :: binary()) -> binary().
encode(AbstractSyntaxUID) ->
    Length = byte_size(AbstractSyntaxUID),
    <<16#30,
      0,
      Length:16,
      AbstractSyntaxUID/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes an Abstract Syntax UID.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(binary()) -> {ok, UID :: binary(), Rest :: binary()} | {error, Data :: binary()}.
decode(Payload = <<16#30, _, Length:16, Data/binary>>) ->
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    AbstractSyntaxString = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, NbBytes - Length),
	    {ok, AbstractSyntaxString, Rest};
	false ->
	    {error, Payload}
    end;
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    V0 = wolfpacs_sop:verification(),
    E0 = encode(V0),
    E1 = <<E0/binary, 42>>,
    I0 = drop_last_byte(E0),
    I1 = <<1,2,3,4>>,
    [ ?_assert(decode(E0) =:= {ok, V0, <<>>}),
      ?_assert(decode(E1) =:= {ok, V0, <<42>>}),
      ?_assert(decode(I0) =:= {error, I0}),
      ?_assert(decode(I1) =:= {error, I1})].
