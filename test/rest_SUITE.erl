-module(rest_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("wolfpacs_types.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1]).
-export([test_workers_resource/1]).

all() -> [test_workers_resource].

init_per_suite(Cfg) ->
    lager_common_test_backend:bounce(debug),
    application:ensure_all_started(wolfpacs),
    Cfg.

end_per_suite(Cfg) ->
    Cfg.

get_workers() ->
    URL = "http://localhost:8080/workers",
    {ok, ResultGet} = httpc:request(URL),
    {_, _, RawBody} = ResultGet,
    jiffy:decode(RawBody, [return_maps]).

post_worker(Worker) ->
    URL = "http://localhost:8080/workers",

    PostBody = jiffy:encode(Worker),
    {ok, Result} = httpc:request(post, {URL, [], "application/json", PostBody}, [], []),
    {_, _, RawBody} = Result,
    jiffy:decode(RawBody, [return_maps]).

test_workers_resource(_Config) ->
    [] = get_workers(),

    W1 = #{<<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"foo">>},

    #{<<"id">> := 0} = post_worker(W1),

    W2 = #{<<"host">> => <<"localhost">>,
	   <<"port">> => 11113,
	   <<"ae">> => <<"bar">>},

    #{<<"id">> := 1} = post_worker(W2),

    Workers = get_workers(),
    2 = length(Workers).
