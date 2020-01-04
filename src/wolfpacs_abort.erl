%%%-------------------------------------------------------------------
%% @doc Abort.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_abort).
-export([encode/2,
	 decode/1]).

-type abort_source() :: 0 | 1 | 2.
-type abort_reason() :: 0 | 2 | 3 | 4 | 5 | 6.

-spec encode(abort_source(), abort_reason()) -> binary().
encode(Source, Reason) ->
    <<16#7,
      0,
      4:32,
      0,
      0,
      Source,
      Reason>>.

-spec decode(binary()) -> {ok, abort_source(), abort_reason(), binary()} | {error, binary()}.
decode(<<16#7, _, 4:32, _, _, Source,  Reason, Rest/binary>>) ->
    {ok, Source, Reason, Rest};
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Source = 2,
    Reason = 3,
    Encoded0 = encode(Source, Reason),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,

    [?_assertEqual(decode(Encoded0), {ok, Source, Reason, <<>>}),
     ?_assertEqual(decode(Encoded1), {ok, Source, Reason, <<42>>}),
     ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
     ?_assertEqual(decode(Incorrect1), {error, Incorrect1})].
