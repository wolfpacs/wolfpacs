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
%% @doc Value Representation Unknown.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_un).
-export([encode/3,
	 decode/3]).

-include("wolfpacs_types.hrl").

encode(Flow, _Strategy, UN) when is_binary(UN) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, UN, 0);
encode(Flow, _Strategy, UN) when is_list(UN) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, list_to_binary(UN), 0).

decode(Flow, _Strategy, UN) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, UN).
