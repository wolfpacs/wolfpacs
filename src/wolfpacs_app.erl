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
%% @doc wolfpacs public API
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    wolfpacs_config:load(),
    Dispatch = cowboy_router:compile(
		 [{'_', [{"/", root_handler, []},
			 {"/clients", clients_handler, []},
			 {"/clients/:client_name/workers", client_workers_handler, []},
			 {"/clients/:client_name/dests", client_dest_handler, []},
			 {"/workers", workers_handler, []},
			 {"/dests", dests_handler, []}
			]}
		 ]),
    cowboy:start_clear(rest_listener,
		       [{port, 8080}],
		       #{env => #{dispatch => Dispatch}}
		      ),
    wolfpacs_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener(rest_listener),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
