-module(workers_handler).
-behaviour(cowboy_rest).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([workers_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, workers_json}
     ], Req, State}.

workers_json(Req, State) ->
    {ok, WorkersObj} = wolfpacs_outside_router:workers(),
    Workers = reformat_workers(WorkersObj),
    {jiffy:encode(Workers), Req, State}.

%%==============================================================================
%% Private
%%==============================================================================

reformat_workers(Workers) ->
    List = maps:to_list(Workers),
    lists:map(fun reformat_worker/1, List).

reformat_worker({Id, {Host, Port, Name}}) ->
    #{<<"id">> => Id,
      <<"host">> => list_to_binary(Host),
      <<"port">> => Port,
      <<"name">> => list_to_binary(Name)}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

reformat_test() ->
    Workers = #{0 => {"localhost",11113,"worker1"},
		1 => {"localhost",11113,"worker2"}},
    Correct = [#{"id" => 0,
		"host" => "localhost",
		"port" => 11113,
		"name" => "worker1"},
	       #{"id" => 1,
		"host" => "localhost",
		"port" => 11113,
		"name" => "worker2"}
	      ],
    Reformated = reformat_workers(Workers),
    ?assertEqual(Reformated, Correct).
