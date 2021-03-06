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
%% @doc wolfpacs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Storage = #{id => wolfpacs_storage,
		start => {wolfpacs_storage, start_link, []}},
    GroupElementsCache = #{id => wolfpacs_group_elements_cache,
			  start => {wolfpacs_group_elements_cache, start_link, []}},
    SenderPool = #{id => wolfpacs_sender_pool,
		   start => {wolfpacs_sender_pool, start_link, []}},
    Clients = #{id => wolfpacs_clients,
		start => {wolfpacs_clients, start_link, []}},
    Workers = #{id => wolfpacs_workers,
		start => {wolfpacs_workers, start_link, []}},
    Dests = #{id => wolfpacs_dests,
		start => {wolfpacs_dests, start_link, []}},
    RouterInsight = #{id => wolfpacs_router_insight,
		      start => {wolfpacs_router_insight, start_link, []}},
    RouteLogic = #{id => wolfpacs_route_logic,
		    start => {wolfpacs_route_logic, start_link, []}},
    OutsideListener = ranch:child_spec(wolfpacs_outside,
				       ranch_tcp, [{port, 11112}],
				       wolfpacs_upper_layer, []),
    InsideListener = ranch:child_spec(wolfpacs_inside,
				      ranch_tcp, [{port, 11113}],
				      wolfpacs_upper_layer, []),
    Children = [Storage,
		GroupElementsCache,
		SenderPool,
		Clients,
		Workers,
		Dests,
		RouterInsight,
		RouteLogic,
		OutsideListener,
		InsideListener],

    {ok, {{one_for_one, 1, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
