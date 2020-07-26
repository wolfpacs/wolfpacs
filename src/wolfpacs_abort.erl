%%%-------------------------------------------------------------------
%% @doc Abort.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_abort).
-export([encode/3,
	 decode/2]).

-type abort_source() :: 0 | 1 | 2.
-type abort_reason() :: 0 | 2 | 3 | 4 | 5 | 6.

-include("wolfpacs_types.hrl").

-spec encode(flow(), abort_source(), abort_reason()) -> binary().
encode(_Flow, Source, Reason) ->
    <<16#7,
      0,
      4:32,
      0,
      0,
      Source,
      Reason>>.

-spec decode(flow(), binary()) -> {ok, abort_source(), abort_reason(), binary()} | error.
decode(_Flow, <<16#7, _, 4:32, _, _, Source,  Reason, Rest/binary>>) ->
    {ok, Source, Reason, Rest};
decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Source = 2,
    Reason = 3,
    Encoded0 = encode(no_flow, Source, Reason),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,

    [ ?_assertEqual(decode(no_flow, Encoded0), {ok, Source, Reason, <<>>})
    , ?_assertEqual(decode(no_flow, Encoded1), {ok, Source, Reason, <<42>>})
    , ?_assertEqual(decode(no_flow, Incorrect0), error)
    , ?_assertEqual(decode(no_flow, Incorrect1), error)
    ].
