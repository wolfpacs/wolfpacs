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
    ListenerSpec = ranch:child_spec(wolfpack, 100,
				    ranch_tcp, [{port, 11112}],
				    wolfpacs_upper_layer, []),

    Children = [ListenerSpec],

    {ok, {{one_for_one, 1, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
