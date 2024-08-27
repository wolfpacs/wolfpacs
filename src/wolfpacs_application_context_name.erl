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
