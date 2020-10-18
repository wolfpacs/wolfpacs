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
%% @doc Workers Handler.
%%
%% @end
%%%-------------------------------------------------------------------

-module(workers_handler).
-behaviour(cowboy_rest).
-include("wolfpacs_types.hrl").
-import(wolfpacs_utils, [b/1]).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Callback Callbacks
-export([workers_to_json/2,
	 workers_from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, workers_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, workers_from_json}
     ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> {true, Req, State};
        <<"POST">> -> {false, Req, State}
    end.

%% Callbacks

workers_to_json(Req, State) ->
    Method = cowboy_req:method(Req),
    workers_to_json(Req, State, Method).

workers_from_json(Req1, State) ->
    {Req2, Body} = read_body_json(Req1),
    #{<<"name">> := Name,
      <<"host">> := Host,
      <<"port">> := Port,
      <<"ae">> := AE} = Body,
    wolfpacs_workers:add(Name, Host, Port, AE),
    Encoded = jiffy:encode(#{<<"name">> => Name}),
    Req3 = cowboy_req:set_resp_body(Encoded, Req2),
    {true, Req3, State}.

%%==============================================================================
%% Private
%%==============================================================================

read_body(Req1, Acc) ->
    case cowboy_req:read_body(Req1) of
	{ok, Data, Req2} ->
	    {Req2, <<Acc/binary, Data/binary>>};
	{more, Data, Req2} ->
	    read_body(Req2, <<Acc/binary, Data/binary>>)
    end.

read_body_json(Req) ->
    {Req2, RawBody} = read_body(Req, <<>>),
    {Req2, jiffy:decode(RawBody, [return_maps])}.

workers_to_json(Req, State, <<"GET">>) ->
    {ok, WorkersObj} = wolfpacs_workers:all(),
    Workers = reformat_workers(WorkersObj),
    {jiffy:encode(Workers), Req, State}.

reformat_workers(Workers) ->
    lists:map(fun reformat_worker/1, Workers).

reformat_worker({Name, #wolfpacs_remote{host=Host, port=Port, ae=AE}}) ->
    #{ <<"name">> => Name
     , <<"host">> => b(Host)
     , <<"port">> => Port
     , <<"ae">> => b(AE)
     }.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
