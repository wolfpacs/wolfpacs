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
%% @doc Destinations Handler.
%%
%% @end
%%%-------------------------------------------------------------------

-module(dests_handler).
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
-export([dests_to_json/2,
	 dests_from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, dests_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, dests_from_json}
     ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> {true, Req, State};
        <<"POST">> -> {false, Req, State}
    end.

%% Callbacks

dests_to_json(Req, State) ->
    Method = cowboy_req:method(Req),
    dests_to_json(Req, State, Method).

dests_from_json(Req1, State) ->
    {Req2, Body} = read_body_json(Req1),
    #{<<"name">> := Name,
      <<"host">> := Host,
      <<"port">> := Port,
      <<"ae">> := AE} = Body,
    wolfpacs_dests:add(Name, Host, Port, AE),
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

dests_to_json(Req, State, <<"GET">>) ->
    {ok, DestsObj} = wolfpacs_dests:all(),
    Dests = reformat_dests(DestsObj),
    {jiffy:encode(Dests), Req, State}.

reformat_dests(Dests) ->
    lists:map(fun reformat_dest/1, Dests).

reformat_dest({Name, #wolfpacs_remote{host=Host, port=Port, ae=AE}}) ->
    #{ <<"name">> => Name
     , <<"host">> => b(Host)
     , <<"port">> => Port
     , <<"ae">> => b(AE)
     }.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
