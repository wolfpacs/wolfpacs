-module(workers_handler).
-behaviour(cowboy_rest).
-include("wolfpacs_types.hrl").


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
    {RawBody, Req2} = cowboy_req:read_urlencoded_body(Req1),
    lager:warning("BODY: ~p", [RawBody]),
    Body = jiffy:decode(RawBody),
    lager:warning("BODY: ~p", [Body]),
    {jiffy:encode(#{}), Req2, State}.

%%==============================================================================
%% Private
%%==============================================================================

workers_to_json(Req, State, <<"GET">>) ->
    {ok, WorkersObj} = wolfpacs_outside_router:workers(),
    Workers = reformat_workers(WorkersObj),
    {jiffy:encode(Workers), Req, State};
workers_to_json(Req, State, <<"POST">>) ->
    {RawBody, Req2} = cowboy_rest:body(Req),
    Body = jiffy:decode(RawBody),
    lager:warning("JFOFOF ~p", [Body]),
    {jiffy:encode(#{}), Req2, State}.

reformat_workers(Workers) ->
    lists:map(fun reformat_worker/1, Workers).

reformat_worker(#wolfpacs_worker{id=Id, host=Host, port=Port, ae=AE, state=State}) ->
    #{<<"id">> => Id,
      <<"host">> => list_to_binary(Host),
      <<"port">> => Port,
      <<"ae">> => list_to_binary(AE),
      <<"state">> => list_to_binary(atom_to_list(State))
     }.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

reformat_worker_test() ->
    W1 = #wolfpacs_worker{id=1,
			  host="localhost",
			  port=11113,
			  ae="foo",
			  state=unseen},
    Correct = #{ <<"ae">> => <<"foo">>
	       , <<"host">> => <<"localhost">>
	       , <<"id">> => 1
	       , <<"port">> => 11113
	       , <<"state">> => <<"unseen">>
	       },
    Res = reformat_worker(W1),
    ?assertEqual(Res, Correct).

reformat_workers_test() ->
    W1 = #wolfpacs_worker{id=1,
			  host="localhost",
			  port=11113,
			  ae="foo",
			  state=unseen},
    W2 = #wolfpacs_worker{id=2,
			  host="localhost",
			  port=11113,
			  ae="bar",
			  state=online},

    C1 = #{ <<"ae">> => <<"foo">>
	  , <<"host">> => <<"localhost">>
	  , <<"id">> => 1
	  , <<"port">> => 11113
	  , <<"state">> => <<"unseen">>
	  },
    C2 = #{ <<"ae">> => <<"bar">>
	  , <<"host">> => <<"localhost">>
	  , <<"id">> => 2
	  , <<"port">> => 11113
	  , <<"state">> => <<"online">>
	  },

    Workers = [W1, W2],
    Reformated = reformat_workers(Workers),
    ?assertEqual(Reformated, [C1, C2]).
