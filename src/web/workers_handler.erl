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
    {Req2, Body} = read_body_json(Req1),
    #{<<"host">> := Host, <<"port">> := Port, <<"ae">> := AE} = Body,
    {ok, Id} = wolfpacs_outside_router:add_worker(Host, Port, AE),
    Encoded = jiffy:encode(#{<<"id">> => Id}),
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
    {ok, WorkersObj} = wolfpacs_outside_router:workers(),
    Workers = reformat_workers(WorkersObj),
    {jiffy:encode(Workers), Req, State}.

reformat_workers(Workers) ->
    lists:map(fun reformat_worker/1, Workers).

reformat_worker(#wolfpacs_worker{id=Id, host=Host, port=Port, ae=AE, state=State}) ->
    #{<<"id">> => Id,
      <<"host">> => b(Host),
      <<"port">> => Port,
      <<"ae">> => b(AE),
      <<"state">> => list_to_binary(atom_to_list(State))
     }.

b(Data) when is_binary(Data) ->
    Data;
b(List) when is_list(List) ->
    list_to_binary(List).

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

b_test_() ->
    [ ?_assertEqual(b("foo"), <<"foo">>)
    , ?_assertEqual(b(<<"foo">>), <<"foo">>)
    ].
