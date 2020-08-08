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
			 {"/workers", workers_handler, []}
			]}
		 ]),
    {ok, _} = cowboy:start_clear(rest_listener,
				 [{port, 8080}],
				 #{env => #{dispatch => Dispatch}}
				),
    wolfpacs_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
