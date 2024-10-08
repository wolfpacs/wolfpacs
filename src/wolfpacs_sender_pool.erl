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
%% @doc Sender Pool.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_sender_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(PARALLEL_SENDS, 25).
-include("wolfpacs_types.hrl").

-export([start_link/0,
	 stop/0,
	 send/2,
	 done/0,
	 size/0,
	 info/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-record(sender_info, { remote :: #wolfpacs_remote{}
		     , dataset :: map()
		     , retries :: integer()
		     }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

send(Remote, DataSet) ->
    SenderInfo = #sender_info{remote = Remote,
			      dataset = DataSet,
			      retries = 0},
    gen_server:cast(?MODULE, {send, SenderInfo}).

done() ->
    gen_server:cast(?MODULE, done).

retry(SenderInfo) ->
    #sender_info{retries = Retries} = SenderInfo,
    gen_server:cast(?MODULE, {retry, SenderInfo#sender_info{retries = Retries + 1}}).

size() ->
    gen_server:call(?MODULE, size).

info() ->
    gen_server:call(?MODULE, info).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    Queue = queue:new(),
    {ok, Queue}.

handle_call(size, _From, Queue) ->
    {reply, {ok, queue:len(Queue)}, Queue};

handle_call(info, _From, Queue) ->
    F = fun(#sender_info{remote=Remote, retries=Retries}) ->
		{Remote, Retries}
	end,
    Items = queue:to_list(Queue),
    Info = lists:map(F, Items),
    {reply, {ok, Info}, Queue};

handle_call(What, _From, Queue) ->
    {reply, {error, What}, Queue}.

handle_cast({send, SenderInfo}, Queue) ->
    {noreply, queue:in(SenderInfo, Queue), 0};

handle_cast({retry, SenderInfo}, Queue) ->
    {noreply, queue:in(SenderInfo, Queue), 0};

handle_cast(done, Queue) ->
    Len = queue:len(Queue),
    handle_timeout(Queue, Len == 0, Len < ?PARALLEL_SENDS);

handle_cast(_What, Queue) ->
    {noreply, Queue}.

handle_info(timeout, Queue) ->
    Len = queue:len(Queue),
    handle_timeout(Queue, Len == 0, Len < ?PARALLEL_SENDS);

handle_info(_What, Queue) ->
    {noreply, Queue}.

terminate(_Reason, _Queue) ->
    ok.

code_change(_Vsn, Queue, _Extra) ->
    {ok, Queue}.

%%==============================================================================
%% Private
%%==============================================================================

handle_timeout(Queue, true, _) ->
    {noreply, Queue, hibernate};
handle_timeout(QueueIn, false, true) ->
    {{value, SenderInfo}, QueueOut} = queue:out(QueueIn),
    spawn(fun() ->
		  #sender_info{ remote = Remote
			      , dataset = DataSet
			      , retries = Retries } = SenderInfo,
		  logger:debug("[SenderPool] Retry: ~p", [Retries]),
		  {ok, Sender} = wolfpacs_sender:start_link(Remote),
		  case wolfpacs_sender:send(Sender, DataSet) of
		      ok ->
			  logger:debug("[SenderPool] Successfully send dataset"),
			  wolfpacs_sender:stop(Sender),
			  done();
		      Error ->
			  logger:warning("[SenderPool] Error: ~p", [Error]),
			  timer:sleep(min(Retries * 1000, 60000)),
			  retry(SenderInfo)
		  end
	  end),
    {noreply, QueueOut, 0};
handle_timeout(Queue, _, _) ->
    {noreply, Queue, 100}.
