%%%-------------------------------------------------------------------
%% @doc Client Workers Handler.
%%
%% @end
%%%-------------------------------------------------------------------

-module(client_workers_handler).
-behaviour(cowboy_rest).
-include("wolfpacs_types.hrl").

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Callback Callbacks
-export([client_workers_to_json/2,
	 client_workers_from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, client_workers_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, client_workers_from_json}
     ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> {true, Req, State};
        <<"POST">> -> {false, Req, State}
    end.

%% Callbacks

client_workers_to_json(Req, State) ->
    Name = cowboy_req:binding(client_name, Req),
    {ok, WorkersObj} = wolfpacs_clients:workers_for_name(Name),
    Workers = lists:sort(reformat_workers(WorkersObj)),
    {jiffy:encode(Workers), Req, State}.

client_workers_from_json(Req1, State) ->
    ClientName = cowboy_req:binding(client_name, Req1),
    {Req2, Body} = read_body_json(Req1),
    #{<<"name">> := WorkerName} = Body,
    wolfpacs_clients:assoc_worker(ClientName, WorkerName),
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

reformat_workers(Workers) ->
    lists:map(fun reformat_worker/1, Workers).

reformat_worker(Worker) ->
    Worker.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
