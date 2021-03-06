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
