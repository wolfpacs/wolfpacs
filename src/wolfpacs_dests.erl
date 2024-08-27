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
%% @doc Destinations.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_dests).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("wolfpacs_types.hrl").

-export([start_link/0,
	 stop/0,
	 reset/0]).
-export([add/4,
	 remote/1,
	 remotes/1,
	 all/0]).
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

remote(Name) ->
    gen_server:call(?MODULE, {remote, b(Name)}).

remotes(Names) ->
    [remote(Name) || Name <- Names].

all() ->
    gen_server:call(?MODULE, all).

%%=============================================================================
%% Behaviour callbacks
%%=============================================================================

init(_) ->
    {ok, #{}}.

handle_call({remote, Name}, _From, Remotes) ->
    case maps:get(Name, Remotes, missing) of
	missing ->
	    {reply, {error, remote_missing}, Remotes};
	Remote ->
	    {reply, {ok, Remote}, Remotes}
    end;

handle_call(all, _From, Remotes) ->
    {reply, {ok, maps:to_list(Remotes)}, Remotes};

handle_call(What, _From, Events) ->
    {reply, {error, What}, Events}.

handle_cast(reset, _Remotes) ->
    {noreply, #{}};

handle_cast({add, Name, Remote}, Remotes) ->
    {noreply, Remotes#{Name => Remote}};

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

    [ ?_assertEqual(remote("R1"), {ok, R1})
    , ?_assertEqual(remote("R2"), {ok, R2})
    , ?_assertEqual(remotes(["R1", "R2"]), [{ok, R1}, {ok, R2}])
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
