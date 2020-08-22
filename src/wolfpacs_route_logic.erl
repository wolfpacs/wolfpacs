%%%-------------------------------------------------------------------
%% @doc Route Logic Commands
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_route_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("wolfpacs_types.hrl").

-export([start_link/0,
	 stop/0]).
-export([allow/1]).
-export([pick_worker/2,
	 pick_destination/1]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

allow(ClientAE) ->
    gen_server:call(?MODULE, {allow, ClientAE}).

pick_worker(_, missing) ->
    {error, no_worker_for_missing_studyuid};

pick_worker(ClientAE, StudyUID) ->
    gen_server:call(?MODULE, {pick_worker, trim(ClientAE), StudyUID}).

pick_destination(StudyUID) ->
    gen_server:call(?MODULE, {pick_dest, StudyUID}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    {ok, [allow_all]}.

handle_call({allow, _AE}, _From, Events) ->
    {reply, {ok, true}, Events};

handle_call({pick_worker, ClientAE, StudyUID}, _From, Events) ->
    case wolfpacs_clients:workers_for_ae(ClientAE) of
	{ok, WorkerNames} ->
	    MaybeWorkers = wolfpacs_workers:remotes(WorkerNames),
	    Workers = lists:filter(fun ok_p/1, MaybeWorkers),

	    case Workers of
		[] ->
		    {reply, {error, no_workers_available}, Events};
		[{ok, Worker}|_] ->
		    wolfpacs_clients:assoc_studyuid(ClientAE, StudyUID),
		    {reply, {ok, Worker}, Events}
	    end;
	_ ->
	    {reply, {error, no_workers_for_client}, Events}
    end;

handle_call({pick_dest, StudyUID}, _From, State) ->
    Result = wolfpacs_clients:dest_for_studyuid(StudyUID),
    {reply, Result, State};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(Event, Events) ->
    {noreply, [Event|Events]}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%=============================================================================
%% Private
%%=============================================================================

trim(Item) when is_binary(Item) ->
    trim(binary_to_list(Item));
trim(Item) ->
    string:strip(string:strip(Item, right, 0), right, 32).

ok_p({ok, _}) ->
    true;
ok_p(_) ->
    false.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

single_test_() ->
    start_link(),
    wolfpacs_clients:start_link(),
    wolfpacs_workers:start_link(),
    wolfpacs_dests:start_link(),

    wolfpacs_clients:add("C", "C_AE"),
    wolfpacs_workers:add("W", "localhost", 11113, "W_AE"),
    wolfpacs_dests:add("D", "localhost", 1235, "D_AE"),

    wolfpacs_clients:assoc_worker("C", "W"),
    wolfpacs_clients:assoc_dest("C", "D"),

    W = #wolfpacs_remote{host= <<"localhost">>, port=11113, ae= <<"W_AE">>},
    D = #wolfpacs_remote{host= <<"localhost">>, port=1235, ae= <<"D_AE">>},

    [ ?_assertEqual(wolfpacs_workers:remote("W"), {ok, W})
    , ?_assertEqual(wolfpacs_dests:remote("D"), {ok, D})
    , ?_assertEqual(pick_worker("C_AE", "X"), {ok, W})
    , ?_assertEqual(pick_destination("X"), {ok, D})
    ].
