%%%-------------------------------------------------------------------
%% @doc Application Context Name
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_application_context_name).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), binary()) -> binary().
encode(Flow, Name) ->
    Length = byte_size(Name),
    wolfpacs_flow:generated(Flow, ?MODULE, 32 + Length),
    <<16#10,
      0,
      Length:16,
      Name/binary>>.

-spec decode(flow(), binary()) -> {ok, binary(), binary()} | error.
decode(Flow, <<16#10, _, Length:16, Data/binary>>) ->
    wolfpacs_flow:good(Flow, ?MODULE, "found correct header"),
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    wolfpacs_utils:split(Data, Length);
	_ ->
	    error
    end;
decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    V0 = <<"1.2.840.10008.3.1.1.1">>,
    E0 = encode(Flow, V0),
    E1 = <<E0/binary, 42>>,
    I0 = wolfpacs_utils:drop_last_byte(E0),
    I1 = <<1,2,3,4>>,
    [ ?_assertEqual(decode(Flow, E0), {ok, V0, <<>>})
    , ?_assertEqual(decode(Flow, E1), {ok, V0, <<42>>})
    , ?_assertEqual(decode(Flow, I0), error)
    , ?_assertEqual(decode(Flow, I1), error)
    ].
