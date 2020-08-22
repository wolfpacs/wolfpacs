-module(testutils).
-export([read_testfile/2,
	 read_dataset/1,
	 read_dataset/2,
	 get_resource/1,
	 post_resource/2]).
-export([get_client_resource/2,
	 post_client_resource/3]).

-include_lib("common_test/include/ct.hrl").

read_testfile(Config, Filename) ->
    filename:join([?config(data_dir, Config), Filename]).

read_dataset(Filename) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    read_dataset(Flow, Filename).

read_dataset(Flow, Filename) ->
    Strategy = {explicit, little},
    {ok, Content} = file:read_file(Filename),
    {ok, {_Meta, Info}, <<>>} = wolfpacs_file_format:decode(Flow, Strategy, Content),
    Info.

get_resource(Endpoint) ->
    URL = "http://localhost:8080" ++ Endpoint,
    {ok, ResultGet} = httpc:request(URL),
    {_, _, RawBody} = ResultGet,
    jiffy:decode(RawBody, [return_maps]).

get_client_resource(Endpoint, Client) ->
    URL = "http://localhost:8080/clients/" ++ Client ++ Endpoint,
    {ok, ResultGet} = httpc:request(URL),
    {_, _, RawBody} = ResultGet,
    jiffy:decode(RawBody, [return_maps]).

post_client_resource(Endpoint, Client, Item) ->
    URL = "http://localhost:8080/clients/" ++ Client ++ Endpoint,
    PostBody = jiffy:encode(Item),
    {ok, _Result} = httpc:request(post, {URL, [], "application/json", PostBody}, [], []).

post_resource(Endpoint, Item) ->
    URL = "http://localhost:8080" ++ Endpoint,
    PostBody = jiffy:encode(Item),
    {ok, Result} = httpc:request(post, {URL, [], "application/json", PostBody}, [], []),
    {_, _, RawBody} = Result,
    jiffy:decode(RawBody, [return_maps]).
