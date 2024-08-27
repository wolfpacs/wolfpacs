%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
%%
%% @doc Workers.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_workers).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(MAX_LOAD, 1000000).
-include("wolfpacs_types.hrl").

-export([start_link/0,
	 stop/0,
	 reset/0,
	 debug/0]).
-export([add/4,
	 remote/1,
	 remotes/1,
	 all/0]).
-export([inc_load/1,
	 dec_load/1,
	 load/1,
	 loads/1]).
-export([name_for_remote/1]).
-export([pause/1,
	 unpause/1]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%=============================================================================
%% API
%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

reset() ->
    gen_server:cast(?MODULE, reset).

add(Name, Host, Port, AE) ->
    Remote = #wolfpacs_remote{host=b(Host), port=Port, ae=b(AE)},
    gen_server:cast(?MODULE, {add, b(Name), Remote}).

inc_load(Remote=#wolfpacs_remote{}) ->
    case name_for_remote(Remote) of
	{ok, Name} ->
	    inc_load(Name);
	_ ->
	    logger:warning("[Workers] Unable to incease load"),
	    error
    end;
inc_load(Name) ->
    gen_server:cast(?MODULE, {inc_load, b(Name)}).

dec_load(Remote=#wolfpacs_remote{}) ->
    case name_for_remote(Remote) of
	{ok, Name} ->
	    dec_load(Name);
	_ ->
	    logger:warning("[Workers] Unable to decrease load"),
	    error
    end;
dec_load(Name) ->
    gen_server:cast(?MODULE, {dec_load, b(Name)}).

load(Name) ->
    gen_server:call(?MODULE, {load, b(Name)}).

loads(Names) ->
    Loads = [{Name, load(Name)} || Name <- Names],
    maps:from_list(Loads).

name_for_remote(Remote) ->
    gen_server:call(?MODULE, {name_for_remote, Remote}).

remote(Name) ->
    gen_server:call(?MODULE, {remote, b(Name)}).

remotes(Names) ->
    AllRemotes = [remote(Name) || Name <- Names],

    OkPred = fun({ok, _, _, false}) ->
		   true;
	      (_) ->
		   false
	   end,
    OkRemotes = lists:filter(OkPred, AllRemotes),

    lists:sort(fun worker_sorter/2, OkRemotes).

pause(Name) ->
    gen_server:cast(?MODULE, {pause, b(Name)}).

unpause(Name) ->
    gen_server:cast(?MODULE, {unpause, b(Name)}).

all() ->
    gen_server:call(?MODULE, all).

debug() ->
    gen_server:call(?MODULE, debug).

%%=============================================================================
%% Behaviour callbacks
%%=============================================================================

-record(state, {remotes :: map(), loads :: map(), paused :: map()}).

init(_) ->
    {ok, #state{remotes=#{}, loads=#{}, paused=#{}}}.

handle_call(debug, _From, State) ->
    {reply, {ok, State}, State};

handle_call({remote, Name}, _From, State) ->
    #state{remotes=Remotes, loads=Loads, paused=PausedNames} = State,
    case maps:get(Name, Remotes, missing) of
	missing ->
	    {reply, {error, remote_missing}, State};
	Remote ->
	    Load = maps:get(Name, Loads, ?MAX_LOAD),
	    Paused = maps:get(Name, PausedNames, false),
	    {reply, {ok, Remote, Load, Paused}, State}
    end;

handle_call(all, _From, State=#state{remotes=Remotes}) ->
    {reply, {ok, maps:to_list(Remotes)}, State};

handle_call({load, Name}, _From, State=#state{loads=Loads}) ->
    case maps:get(Name, Loads, missing) of
	missing ->
	    {reply, {error, worker_missing}, State};
	Load ->
	    {reply, {ok, Load}, State}
    end;

handle_call({name_for_remote, Remote}, _From, State=#state{remotes=Map}) ->
    Remotes = maps:to_list(Map),
    Pred = fun({_Name, R}) when R == Remote ->
		   true;
	      ({_Name, _Other}) ->
		   false
	   end,
    case lists:filter(Pred, Remotes) of
	[{Name, Remote}] ->
	    {reply, {ok, Name}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

handle_call(What, _From, State) ->
    {reply, {error, What, State}, State}.

handle_cast(reset, _State) ->
    {noreply, #state{remotes=#{}, loads=#{}, paused=#{}}};

handle_cast({add, Name, Remote}, State) ->
    #state{remotes=Remotes, loads=Loads} = State,
    NewRemotes = maps:put(Name, Remote, Remotes),
    NewLoads = maps:put(Name, 0, Loads),
    {noreply, State#state{remotes=NewRemotes, loads=NewLoads}};

handle_cast({inc_load, Name}, State=#state{loads=Loads}) ->
    Load = maps:get(Name, Loads, 0),
    {noreply, State#state{loads=maps:put(Name, Load + 1, Loads)}};

handle_cast({dec_load, Name}, State=#state{loads=Loads}) ->
    Load = maps:get(Name, Loads, 0),
    {noreply, State#state{loads=maps:put(Name, Load - 1, Loads)}};

handle_cast({pause, Name}, State) ->
    #state{ paused=Paused } = State,
    {noreply, State#state{paused=Paused#{Name => true}}};

handle_cast({unpause, Name}, State) ->
    #state{ paused=Paused } = State,
    {noreply, State#state{paused=maps:remove(Name, Paused)}};

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

b(String) when is_list(String) ->
    list_to_binary(String);
b(Data) when is_binary(Data) ->
    Data.

worker_sorter({ok, #wolfpacs_remote{ae=AE0}, Load, _}, {ok, #wolfpacs_remote{ae=AE1}, Load, _}) ->
    AE0 =< AE1;
worker_sorter({ok, _, Load0, _}, {ok, _, Load1, _}) ->
    Load0 =< Load1.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

minimal_test_() ->
    start_link(),

    add("R1", "localhost", 1, "R1_AE"),
    add("R2", "localhost", 2, "R2_AE"),

    R1 = #wolfpacs_remote{host= <<"localhost">>, port=1, ae= <<"R1_AE">>},
    R2 = #wolfpacs_remote{host= <<"localhost">>, port=2, ae= <<"R2_AE">>},
    R3 = #wolfpacs_remote{host= <<"localhost">>, port=3, ae= <<"R3_AE">>},

    inc_load(R1), inc_load(R1), inc_load(R1),
    inc_load(R2), inc_load(R2),

    [ ?_assertEqual(remote("R1"), {ok, R1, 3, false})
    , ?_assertEqual(remote("R2"), {ok, R2, 2, false})
    , ?_assertEqual(name_for_remote(R1), {ok, <<"R1">>})
    , ?_assertEqual(name_for_remote(R2), {ok, <<"R2">>})
    , ?_assertEqual(name_for_remote(R3), {error, not_found})
    , ?_assertEqual(remotes(["R1", "R2", "RX"]), [{ok, R2, 2, false}, {ok, R1, 3, false}])
    ].

inc_dec_test_() ->
    start_link(),

    add("R1", "localhost", 1, "R1_AE"),
    add("R2", "localhost", 2, "R2_AE"),

    R1 = #wolfpacs_remote{host= <<"localhost">>, port=1, ae= <<"R1_AE">>},
    R2 = #wolfpacs_remote{host= <<"localhost">>, port=2, ae= <<"R2_AE">>},
    R3 = #wolfpacs_remote{host= <<"localhost">>, port=3, ae= <<"R3_AE">>},

    [ ?_assertEqual(inc_load(R1), ok)
    , ?_assertEqual(inc_load(R1), ok)
    , ?_assertEqual(dec_load(R1), ok)
    , ?_assertEqual(inc_load(R2), ok)
    , ?_assertEqual(dec_load(R2), ok)
    , ?_assertEqual(inc_load(R3), error)
    , ?_assertEqual(dec_load(R3), error)
    , ?_assertEqual(load("R1"), {ok, 1})
    , ?_assertEqual(load("R2"), {ok, 0})
    , ?_assertEqual(load("R3"), {error, worker_missing})
    , ?_assertEqual(loads(["R1", "R2"]), #{"R1" => {ok, 1}, "R2" => {ok,  0}})
    ].

pause_unpause_test_() ->
    [
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
