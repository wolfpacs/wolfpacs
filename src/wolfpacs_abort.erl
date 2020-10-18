%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Abort.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_abort).
-export([encode/3,
	 decode/2]).

-include("wolfpacs_types.hrl").

-spec encode(Flow :: flow(), AbortSource :: byte(), AbortReason :: byte()) -> binary().
encode(Flow, Source, Reason) ->
    wolfpacs_flow:success(Flow, ?MODULE),
    wolfpacs_flow:generated(Flow, ?MODULE, 10),
    <<16#7,
      0,
      4:32,
      0,
      0,
      Source,
      Reason>>.

-spec decode(flow(), binary()) -> {ok, AbortSource :: byte(), AbortReason :: byte(), binary()} | error.
decode(Flow, <<16#7, _, 4:32, _, _, Source,  Reason, Rest/binary>>) ->
    wolfpacs_flow:success(Flow, ?MODULE),
    wolfpacs_flow:consumed(Flow, ?MODULE, 10),
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
