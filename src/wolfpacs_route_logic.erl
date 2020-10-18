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
    {ok, #{}}.

handle_call({allow, _AE}, _From, State) ->
    {reply, {ok, true}, State};

handle_call({pick_worker, ClientAE, StudyUID}, _From, State) ->
    MaybeWorker = maps:get(StudyUID, State, missing),
    priv_pick_worker(MaybeWorker, ClientAE, StudyUID, State);

handle_call({pick_dest, StudyUID}, _From, State) ->
    Result = wolfpacs_clients:dest_for_studyuid(StudyUID),
    {reply, Result, State};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

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

priv_pick_worker(missing, ClientAE, StudyUID, State) ->
    case wolfpacs_clients:workers_for_ae(ClientAE) of
	{ok, WorkerNames} ->
	    case wolfpacs_workers:remotes(WorkerNames) of
		[] ->
		    {reply, {error, no_workers_available}, State};
		[{ok, Worker, _Load}|_] ->
		    wolfpacs_workers:inc_load(Worker),
		    wolfpacs_clients:assoc_studyuid(ClientAE, StudyUID),
		    {reply, {ok, Worker}, State#{StudyUID => Worker}}
	    end;
	_ ->
	    {reply, {error, no_workers_for_client}, State}
    end;
priv_pick_worker(Worker, _ClientAE, _StudyUID, State) ->
    {reply, {ok, Worker}, State}.

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

    [ ?_assertEqual(wolfpacs_workers:remote("W"), {ok, W, 0})
    , ?_assertEqual(wolfpacs_dests:remote("D"), {ok, D})
    , ?_assertEqual(pick_worker("C_AE", "X"), {ok, W})
    , ?_assertEqual(pick_destination("X"), {ok, D})
    , ?_assertEqual(stop(), ok)
    ].


advanced_worker_pick_test_() ->
    start_link(),

    wolfpacs_clients:start_link(),
    wolfpacs_workers:start_link(),
    wolfpacs_dests:start_link(),

    wolfpacs_clients:add("C", "C_AE"),

    wolfpacs_workers:add("W1", "localhost", 11111, "W1_AE"),
    wolfpacs_workers:add("W2", "localhost", 11112, "W2_AE"),
    wolfpacs_workers:add("W3", "localhost", 11113, "W3_AE"),

    wolfpacs_clients:assoc_worker("C", "W1"),
    wolfpacs_clients:assoc_worker("C", "W2"),
    wolfpacs_clients:assoc_worker("C", "W3"),

    W1 = #wolfpacs_remote{host= <<"localhost">>, port=11111, ae= <<"W1_AE">>},
    W2 = #wolfpacs_remote{host= <<"localhost">>, port=11112, ae= <<"W2_AE">>},
    W3 = #wolfpacs_remote{host= <<"localhost">>, port=11113, ae= <<"W3_AE">>},

    [ ?_assertEqual(pick_worker("C_AE", <<"StudyUID1">>), {ok, W1})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID2">>), {ok, W2})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID3">>), {ok, W3})

    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID1">>), {ok, W1})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID1">>), {ok, W1})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID1">>), {ok, W1})

    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID2">>), {ok, W2})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID2">>), {ok, W2})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID2">>), {ok, W2})

    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID3">>), {ok, W3})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID3">>), {ok, W3})
    , ?_assertEqual(pick_worker("C_AE", <<"StudyUID3">>), {ok, W3})
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
