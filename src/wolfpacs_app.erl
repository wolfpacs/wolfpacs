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
