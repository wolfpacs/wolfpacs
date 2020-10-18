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
%% @doc Client Destinations Handler.
%%
%% @end
%%%-------------------------------------------------------------------

-module(client_dest_handler).
-behaviour(cowboy_rest).
-include("wolfpacs_types.hrl").

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Callback Callbacks
-export([client_dest_to_json/2,
	 client_dest_from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, client_dest_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, client_dest_from_json}
     ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> {true, Req, State};
        <<"POST">> -> {false, Req, State}
    end.

%% Callbacks

client_dest_to_json(Req, State) ->
    Name = cowboy_req:binding(client_name, Req),
    {ok, Dest} = wolfpacs_clients:dest_for_name(Name),
    {jiffy:encode(Dest), Req, State}.

client_dest_from_json(Req1, State) ->
    ClientName = cowboy_req:binding(client_name, Req1),
    {Req2, Body} = read_body_json(Req1),
    #{<<"name">> := WorkerName} = Body,
    wolfpacs_clients:assoc_dest(ClientName, WorkerName),
    Encoded = jiffy:encode(#{<<"msg">> => <<"ok">>}),
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

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
