%%%-------------------------------------------------------------------
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
