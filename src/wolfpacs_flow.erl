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
%% @doc Flow
%%
%% Flow helps track the flow through wolfpacs.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_flow).
-behaviour(gen_server).

%% API
-export([start_link/0,
	 stop/1,
	 reset/1,
	 ge/4,
	 generated/3,
	 consumed/3,
	 start_encode/2,
	 start_decode/2,
	 success/2,
	 failed/3,
	 good/3,
	 bad/3,
	 expected_16/3,
	 expected_32/3,
	 events/1]).

%% Behaviour
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
    gen_server:start_link(?MODULE, [], []).

stop(Flow) ->
    gen_server:stop(Flow).

ge(Flow, Module, Group, Element) ->
    good(Flow, Module, io_lib:format("(~.16B, ~.16B)", [Group, Element])).

generated(Flow, Module, Data) when is_binary(Data) ->
    generated(Flow, Module, byte_size(Data));

generated(Flow, Module, List) when is_list(List) ->
    generated(Flow, Module, length(List));

generated(Flow, Module, NumberOfBytes) ->
    common_cast(Flow, {generated, Module, NumberOfBytes}).

consumed(Flow, Module, NumberOfBytes) ->
    common_cast(Flow, {consumed, Module, NumberOfBytes}).

reset(Flow) ->
    common_cast(Flow, reset).

start_encode(Flow, Module) ->
    common_cast(Flow, {good, Module, "start encode"}).

start_decode(Flow, Module) ->
    common_cast(Flow, {good, Module, "start decode"}).

success(Flow, Module) ->
    common_cast(Flow, {good, Module, "success"}).

failed(Flow, Module, Info) ->
    common_cast(Flow, {bad, Module, Info}).

good(Flow, Module, Info) ->
    common_cast(Flow, {good, Module, Info}).

expected_16(Flow, Module, <<Bytes:16/bitstring, _Rest/binary>>) ->
    common_cast(Flow, {expected, Module, Bytes});
expected_16(Flow, Module, _Data) ->
    common_cast(Flow, {expected, Module, not_enough_data}).

expected_32(Flow, Module, <<Bytes:32/bitstring, _Rest/binary>>) ->
    common_cast(Flow, {expected, Module, Bytes});
expected_32(Flow, Module, _Data) ->
    common_cast(Flow, {expected, Module, not_enough_data}).

bad(Flow, Module, Info) ->
    common_cast(Flow, {bad, Module, Info}).

events(Flow) ->
    gen_server:call(Flow, events).

%%------------------------------------------------------------------------------
%% @doc Common Cast protect agasint no_flow
%%
%% @end
%%------------------------------------------------------------------------------

common_cast(no_flow, _) ->
    ok;
common_cast(Flow, Info={bad, Module, Data}) ->
    logger:warning("[~p] ~p: ~p", [Module, bad, Data]),
    gen_server:cast(Flow, Info);
common_cast(Flow, Info={Tag, Module, Data}) ->
    logger:debug("[~p] ~p: ~p", [Module, Tag, Data]),
    gen_server:cast(Flow, Info);
common_cast(Flow, Info) ->
    gen_server:cast(Flow, Info).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(flow_state, {events = [] :: list(),
		     generated = 0 :: integer(),
		     consumed = 0 :: integer()}).

%% @hidden
init(_) ->
    {ok, #flow_state{}}.

handle_call(events, _From, State=#flow_state{events=Events}) ->
    {reply, {ok, Events}, State};

handle_call(What, _From, Flow) ->
    {reply, {error, What}, Flow}.

handle_cast(reset, _) ->
    {noreply, #flow_state{}};

handle_cast({consumed, _Module, _NumberOfBytes}, Flow) ->
    {noreply, Flow};

handle_cast({generated, _Module, _NumberOfBytes}, Flow) ->
    {noreply, Flow};

handle_cast(Event, State=#flow_state{events=Events}) ->
    {noreply, State#flow_state{events=[Event|Events]}}.

handle_info(_What, Flow) ->
    {noreply, Flow}.

terminate(_Reason, _Flow) ->
    ok.

code_change(_Vsn, Flow, _Extra) ->
    {ok, Flow}.

%%-----------------------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

minimal_test() ->
    {ok, Flow} = start_link(),
    {ok, []} = events(Flow),

    ok = expected_16(Flow, "", <<256:16, 1, 2, 3>>),
    {ok, [{expected, "", <<256:16>>}]} = events(Flow),
    ok = expected_16(Flow, "", <<>>),
    {ok, [{expected, "", not_enough_data}|_]} = events(Flow),
    reset(Flow),

    ok = expected_32(Flow, "", <<256:32, 1, 2, 3>>),
    {ok, [{expected, "", <<256:32>>}]} = events(Flow),
    ok = expected_32(Flow, "", <<>>),
    {ok, [{expected, "", not_enough_data}|_]} = events(Flow),
    reset(Flow),

    ok = stop(Flow).
