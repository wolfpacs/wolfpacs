%%%-------------------------------------------------------------------
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
    Authentication = #{id => wolfpacs_authentication,
		start => {wolfpacs_authentication, start_link, []}},
    GroupElementsCache = #{id => wolfpacs_group_elements_cache,
			  start => {wolfpacs_group_elements_cache, start_link, []}},
    SenderPool = #{id => wolfpacs_sender_pool,
		      start => {wolfpacs_sender_pool, start_link, []}},
    RouterInsight = #{id => wolfpacs_router_insight,
		      start => {wolfpacs_router_insight, start_link, []}},
    OutsideRouter = #{id => wolfpacs_outside_router,
		      start => {wolfpacs_outside_router, start_link, []}},
    InsideRouter = #{id => wolfpacs_inside_router,
		      start => {wolfpacs_inside_router, start_link, []}},
    OutsideListener = ranch:child_spec(wolfpacs_outside,
				       ranch_tcp, [{port, 11112}],
				       wolfpacs_upper_layer, []),
    InsideListener = ranch:child_spec(wolfpacs_inside,
				      ranch_tcp, [{port, 11113}],
				      wolfpacs_upper_layer, []),
    Children = [Storage,
		Authentication,
		GroupElementsCache,
		SenderPool,
		RouterInsight,
		OutsideRouter,
		InsideRouter,
		OutsideListener,
		InsideListener],

    {ok, {{one_for_one, 1, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
