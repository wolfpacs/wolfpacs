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
%% @doc Value Representation Application Entity.
%%
%% A string of characters that identifies an Application Entity with
%% leading and trailing spaces (20H) being non-significant.
%% A value consisting solely of spaces shall not be used.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_ae).
-export([encode/3, decode/3]).

encode(Flow, _Strategy, AE) ->
    wolfpacs_vr_common:encode_exact(Flow, ?MODULE, AE, 16, " ").

decode(Flow, _Strategy, Data) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, Data).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = <<"WolfPACS">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).
