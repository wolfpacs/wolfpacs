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
%% @doc Max Length.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_max_length).
-export([encode/1,
	 decode/1]).

-define(LENGTH, 4).

-spec encode(non_neg_integer()) -> binary().
encode(MaxLength) ->
    <<16#51,
      0,
      ?LENGTH:16,
      MaxLength:32>>.

-spec decode(binary()) -> {ok, non_neg_integer(), binary()} | {error, binary()}.
decode(<<16#51, 0, ?LENGTH:16, MaxLength:32, Rest/binary>>) ->
    {ok, MaxLength, Rest};
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    Value = 65536,
    Encoded0 = encode(Value),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = <<1, 2, 3, 4>>,
    [ ?_assertEqual(decode(Encoded0), {ok, Value, <<>>}),
      ?_assertEqual(decode(Encoded1), {ok, Value, <<42>>}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0}) ].
