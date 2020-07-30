-module(wolfpacs_workers).
-behaviour(supervisor).

%% API
-export([start_link/0,
	 add/3,
	 remove/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add(Host, Port, AE) ->
    {ok, Worker} = supervisor:start_child(?MODULE, [Host, Port, AE]),
    ets:insert(?MODULE, {key(Host, Port, AE), Worker}),
    {ok, Worker}.

remove(Host, Port, AE) ->
    case ets:lookup(?MODULE, key(Host, Port, AE)) of
	[{_, Worker}] ->
	    supervisor:terminate_child(?MODULE, Worker),
	    {ok, removed};
	_ ->
	    {error, missing}
    end.

key(Host, Port, AE) ->
    {Host, Port, AE}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ets:new(?MODULE, [set,                                                                                                                         public,                                                                                                                      named_table,                                                                                                                 {read_concurrency, true},                                                                                                    {write_concurrency, true}]),
    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 1,
		 period => 1},

    WorkerSpecs = #{id => worker,
		    restart => transient,
		    start => {wolfpacs_worker, start_link, []}},

    Children = [WorkerSpecs],

    {ok, {SupFlags, Children}}.
