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
