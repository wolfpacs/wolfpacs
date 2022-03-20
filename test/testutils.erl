-module(testutils).
-export([read_testfile/2,
	 read_dataset/1,
	 read_dataset/2]).

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
