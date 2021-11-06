%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%
%% @doc Group Element Cache.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_group_elements_cache).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 stop/0,
	 add/3,
	 get/2,
	 size/0,
	 purge/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

add(Group, Element, VR) ->
    gen_server:cast(?MODULE, {add, {Group, Element}, VR}).

get(Group, Element) ->
    try gen_server:call(?MODULE, {get, {Group, Element}}) of
	Res -> Res
    catch
	_:_ ->
	    "UN"
    end.

size() ->
    gen_server:call(?MODULE, size).

purge() ->
    gen_server:cast(?MODULE, purge).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    {ok, #{}}.

handle_call({get, Key}, _From, Cache) ->
    Value = maps:get(Key, Cache, "UN"),
    {reply, {ok, Value}, Cache};
handle_call(size, _From, Cache) ->
    {reply, {ok, maps:size(Cache)}, Cache};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({add, Key, Value}, Cache) ->
    {noreply, Cache#{Key => Value}};

handle_cast(purge, _Cache) ->
    {noreply, #{}};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

minimal_test_() ->
    _ = start_link(),
    [ ?_assertEqual(size(), {ok, 0})
    , ?_assertEqual(add(1, 2, "AE"), ok)
    , ?_assertEqual(size(), {ok, 1})
    , ?_assertEqual(add(1, 2, "AE"), ok)
    , ?_assertEqual(size(), {ok, 1})
    , ?_assertEqual(add(3, 4, "AS"), ok)
    , ?_assertEqual(size(), {ok, 2})
    , ?_assertEqual(purge(), ok)
    , ?_assertEqual(size(), {ok, 0})
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
