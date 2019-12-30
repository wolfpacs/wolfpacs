%%%-------------------------------------------------------------------
%% @doc Application Context Name
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_application_context_name).
-export([encode/1,
	 decode/1]).
-import(wolfpacs_utils, [drop_last_byte/1]).

-spec encode(binary()) -> binary().
encode(Name) ->
    Length = byte_size(Name),
    <<16#10,
      0,
      Length:16,
      Name/binary>>.

-spec decode(binary()) -> {ok, binary(), binary()} | {error, binary()}.
decode(Payload = <<16#10, _, Length:16, Data/binary>>) ->
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    wolfpacs_utils:split(Data, Length);
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
    V0 = <<"1.2.840.10008.3.1.1.1">>,
    E0 = encode(V0),
    E1 = <<E0/binary, 42>>,
    I0 = drop_last_byte(E0),
    I1 = <<1,2,3,4>>,
    [ ?_assert(decode(E0) =:= {ok, V0, <<>>}),
      ?_assert(decode(E1) =:= {ok, V0, <<42>>}),
      ?_assert(decode(I0) =:= {error, I0}),
      ?_assert(decode(I1) =:= {error, I1})].
