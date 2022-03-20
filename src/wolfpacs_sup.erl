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
				       ranch_tcp, [{port, outside_port()}],
				       wolfpacs_upper_layer, []),
    InsideListener = ranch:child_spec(wolfpacs_inside,
				      ranch_tcp, [{port, inside_port()}],
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

outside_port() ->
    case os:getenv("WOLFPACS_OUTSIDE_PORT", missing) of
        missing ->
            application:get_env(wolfpacs, outside_port, 11112);
        Port ->
            list_to_integer(Port)
    end.

inside_port() ->
    case os:getenv("WOLFPACS_INSIDE_PORT", missing) of
        missing ->
            application:get_env(wolfpacs, inside_port, 11113);
        Port ->
            list_to_integer(Port)
    end.
