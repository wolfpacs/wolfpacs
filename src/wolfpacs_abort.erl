%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
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
