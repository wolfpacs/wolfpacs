%%%-------------------------------------------------------------------
%% @doc Clients.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_clients).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("wolfpacs_types.hrl").

-export([start_link/0,
	 stop/0]).
-export([add/2,
	 assoc_worker/2,
	 assoc_dest/2,
	 assoc_studyuid/2,
	 workers_for_name/1,
	 workers_for_ae/1,
	 dest_for_name/1,
	 dest_for_ae/1,
	 dest_for_studyuid/1,
	 all/0]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-import(wolfpacs_utils, [b/1]).

%%=============================================================================
%% API
%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

add(Name, AE) ->
    gen_server:cast(?MODULE, {add, b(Name), b(trim(AE))}).

assoc_worker(Name, Worker) ->
    gen_server:cast(?MODULE, {assoc_worker, b(Name), b(Worker)}).

assoc_dest(Name, Dest) ->
    gen_server:cast(?MODULE, {assoc_dest, b(Name), b(Dest)}).

assoc_studyuid(AE, StudyUID) ->
    gen_server:cast(?MODULE, {assoc_studyuid, b(AE), b(StudyUID)}).

workers_for_name(Name) ->
    gen_server:call(?MODULE, {workers_for_name, b(Name)}).

workers_for_ae(AE) ->
    gen_server:call(?MODULE, {workers_for_ae, b(trim(AE))}).

dest_for_name(Name) ->
    gen_server:call(?MODULE, {dest_for_name, b(Name)}).

dest_for_ae(AE) ->
    gen_server:call(?MODULE, {dest_for_ae, b(trim(AE))}).

dest_for_studyuid(StudyUID) ->
    gen_server:call(?MODULE, {dest_for_studyuid, b(StudyUID)}).

all() ->
    gen_server:call(?MODULE, all).

%%=============================================================================
%% Behaviour callbacks
%%=============================================================================

-record(state, { names_to_ae = #{} :: map()
	       , ae_to_names = #{} :: map()
	       , ae_to_workers = #{} :: map()
	       , ae_to_dests = #{} :: map()
	       , studyuid_to_dests = #{} :: map()
	       }).

init(_) ->
    {ok, #state{}}.

handle_call({workers_for_name, Name}, _From, State) ->
    #state{ names_to_ae=NamesToAE
	  , ae_to_workers=AEToWorkers } = State,
    AE = maps:get(Name, NamesToAE, missing),
    Workers = maps:get(AE, AEToWorkers, []),
    {reply, {ok, Workers}, State};

handle_call({workers_for_ae, AE}, _From, State) ->
    #state{ ae_to_workers=AEToWorkers } = State,
    Workers = maps:get(AE, AEToWorkers, []),
    {reply, {ok, Workers}, State};

handle_call({dest_for_name, Name}, _From, State) ->
    #state{ names_to_ae=NamesToAE
	  , ae_to_dests=AEToDests } = State,
    AE = maps:get(Name, NamesToAE, missing),
    Dests = maps:get(AE, AEToDests, []),
    {reply, {ok, Dests}, State};

handle_call({dest_for_ae, AE}, _From, State) ->
    #state{ ae_to_dests=AEToDests } = State,
    Dests = maps:get(AE, AEToDests, []),
    {reply, {ok, Dests}, State};

handle_call({dest_for_studyuid, StudyUID}, _From, State) ->
    #state{ studyuid_to_dests=StudyUIDToDests } = State,
    case maps:get(StudyUID, StudyUIDToDests, missing) of
	missing ->
	    {reply, {error, studyuid_not_mapped}, State};
	DestName ->
	    Result = wolfpacs_dests:remote(DestName),
	    {reply, Result, State}
    end;

handle_call(all, _From, State) ->
    #state{ names_to_ae=NamesToAE } = State,
    Result = maps:to_list(NamesToAE),
    {reply, {ok, Result}, State};

handle_call(What, _From, Events) ->
    {reply, {error, What}, Events}.

handle_cast({add, Name, AE}, State) ->
    #state{ names_to_ae=NamesToAE
	  , ae_to_names=AEToNames
	  , ae_to_workers=AEToWorkers
	  , ae_to_dests=AEToDests
	  } = State,
    NewState = #state{names_to_ae=NamesToAE#{Name => AE},
		      ae_to_names=AEToNames#{AE => Name},
		      ae_to_workers=AEToWorkers#{AE => []},
		      ae_to_dests=AEToDests#{AE => []}},
    {noreply, NewState};

handle_cast({assoc_worker, Name, Worker}, State) ->
    #state{ names_to_ae=NamesToAE
	  , ae_to_workers=AEToWorkers } = State,
    AE = maps:get(Name, NamesToAE),
    Workers = maps:get(AE, AEToWorkers, []),
    {noreply, State#state{ae_to_workers=AEToWorkers#{AE => [Worker|Workers]}}};

handle_cast({assoc_dest, Name, Dest}, State) ->
    #state{ names_to_ae=NamesToAE
	  , ae_to_dests=AEToDests } = State,
    AE = maps:get(Name, NamesToAE),
    {noreply, State#state{ae_to_dests=AEToDests#{AE => Dest}}};

handle_cast({assoc_studyuid, AE, StudyUID}, State) ->
    #state{ studyuid_to_dests=StudyUIDToDests
	  , ae_to_dests=AEToDests } = State,
    Dest = maps:get(AE, AEToDests),
    {noreply, State#state{studyuid_to_dests=StudyUIDToDests#{StudyUID => Dest}}};

handle_cast(_What, State) ->
    {noreply, State}.

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

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

minimal_test_() ->
    start_link(),

    add("C", "C_AE"),

    assoc_worker("C", "W1"),
    assoc_worker("C", "W2"),

    assoc_dest("C", "D1"),

    [ ?_assertEqual(workers_for_name("C"), {ok, [<<"W2">>, <<"W1">>]})
    , ?_assertEqual(workers_for_name("C"), workers_for_ae(<<"C_AE">>))
    , ?_assertEqual(dest_for_name("C"), {ok, <<"D1">>})
    , ?_assertEqual(ok, stop())
    ].

studyuid_test_() ->
    start_link(),
    wolfpacs_dests:start_link(),
    ok = wolfpacs_dests:add("D1", "localhost", 1111, "D1_AE"),

    add("C", "C_AE"),
    assoc_worker("C", "W1"),
    assoc_dest("C", "D1"),

    Dest = #wolfpacs_remote{host= <<"localhost">>, port=1111, ae= <<"D1_AE">>},

    [ ?_assertEqual(workers_for_name("C"), {ok, [<<"W1">>]})
    , ?_assertEqual(workers_for_name("C"), workers_for_ae(<<"C_AE">>))
    , ?_assertEqual(dest_for_name("C"), {ok, <<"D1">>})
    , ?_assertEqual(assoc_studyuid("C_AE", "SUID"), ok)
    , ?_assertEqual(dest_for_studyuid("SUID"), {ok, Dest})
    , ?_assertEqual(ok, stop())
    ].

start_stop_test() ->
    start_link(),
    ?assertEqual(stop(), ok).

cast_test() ->
    start_link(),
    gen_server:cast(?MODULE, this_should_not_crash),
    ?assertEqual(stop(), ok).

info_test() ->
    start_link(),
    ?MODULE ! this_should_not_crash,
    ?assertEqual(stop(), ok).

code_change_test() ->
    start_link(),
    ?assertEqual(code_change(1, state, extra), {ok, state}).
