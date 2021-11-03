%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%
%% @doc Value Representation - Common Helper functions.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_common).
-export([encode/4,
	 encode_exact/5,
	 encode_limit/5,
	 decode/3
	]).

-include("wolfpacs_types.hrl").

%%------------------------------------------------------------------------------
%% @doc Encode
%%
%% @end
%%------------------------------------------------------------------------------
encode(Flow, Module, Data, PadChar) ->
    Bytes = wolfpacs_vr_utils:pad(Data, PadChar),
    wolfpacs_flow:generated(Flow, Module, Bytes),
    Bytes.

encode_exact(Flow, Module, Data, Length, PadChar) ->
    Bytes = wolfpacs_vr_utils:exact(Data, Length, PadChar),
    wolfpacs_flow:generated(Flow, Module, Bytes),
    Bytes.

encode_limit(Flow, Module, Data, Length, PadChar) ->
    Padded = wolfpacs_vr_utils:pad(Data, PadChar),
    Limit = Length * value_multiplicity(Data),
    Bytes = wolfpacs_vr_utils:limit(Padded, Limit),
    wolfpacs_flow:generated(Flow, Module, Bytes),
    Bytes.

%%------------------------------------------------------------------------------
%% @doc Decode Binary
%%
%% @end
%%------------------------------------------------------------------------------
-spec decode(flow(), module(), binary()) -> {ok, binary(), binary()}.
decode(Flow, Module, Data) ->
    Bytes = wolfpacs_vr_utils:trim(Data),
    wolfpacs_flow:consumed(Flow, Module, byte_size(Data)),
    {ok, Bytes, <<>>}.

%%==============================================================================
%% Private
%%==============================================================================

backslash(92, Acc) ->
    Acc + 1;
backslash(_, Acc) ->
    Acc.

value_multiplicity(Data) ->
    Str = binary_to_list(Data),
    lists:foldl(fun backslash/2, 1, Str).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Encoded = encode(no_flow, ?MODULE, <<"abc">>, " "),
    ?assertEqual(Encoded, <<"abc ">>).

encode_exact_test_() ->
    Encoded0 = encode_exact(no_flow, ?MODULE, <<"abc">>, 0, " "),
    Encoded1 = encode_exact(no_flow, ?MODULE, <<"abc">>, 2, " "),
    Encoded2 = encode_exact(no_flow, ?MODULE, <<"abc">>, 4, " "),
    Encoded3 = encode_exact(no_flow, ?MODULE, <<"abc">>, 6, " "),
    [ ?_assertEqual(Encoded0, <<"">>)
    , ?_assertEqual(Encoded1, <<"ab">>)
    , ?_assertEqual(Encoded2, <<"abc ">>)
    , ?_assertEqual(Encoded3, <<"abc   ">>)
    ].

value_multiplicity_test_() ->
    [ ?_assertEqual(value_multiplicity(<<"A">>), 1)
    , ?_assertEqual(value_multiplicity(<<"A\\B">>), 2)
    , ?_assertEqual(value_multiplicity(<<"A\\B\\C">>), 3)
    ].
