-module(wolfpacs_utils).
-include_lib("eunit/include/eunit.hrl").
-export([drop_last_byte/1]).

-spec drop_last_byte(binary()) -> binary().
drop_last_byte(Data) ->
    list_to_binary(lists:droplast(binary_to_list(Data))).

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

drop_last_byte_test_() ->
    [?_assert(drop_last_byte(<<1, 2, 3>>) =:= <<1 ,2>>) ].
