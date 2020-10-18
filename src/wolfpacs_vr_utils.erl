%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Value Representation Utilities.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_utils).
-export([pad/2,
	 limit/2,
	 exact/3,
	 trim/1]).

pad(Item, 0) ->
    case has_odd_length(Item) of
	true ->
	    <<Item/binary, 0>>;
	false ->
	    Item
    end;
pad(Item, " ") ->
    case has_odd_length(Item) of
	true ->
	    <<Item/binary, " ">>;
	false ->
	    Item
    end.

limit(Item, Max) when byte_size(Item) > Max ->
    binary:part(Item, 0, Max);
limit(Item, _) ->
    Item.

exact(Item, Target, PadChar) ->
    NbBytes = byte_size(Item),
    exact(Item, Target, PadChar, NbBytes).

trim(Item) ->
    Tmp = binary_to_list(Item),
    Str = string:strip(string:strip(Tmp, right, 0), right, 32),
    list_to_binary(Str).

%%==============================================================================
%% Private
%%==============================================================================

has_odd_length(Item) when is_binary(Item) ->
    (byte_size(Item) band 1) == 1;

has_odd_length(Item) when is_list(Item) ->
    (length(Item) band 1) == 1.

exact(Item, Target, _PadChar, Target) ->
    Item;
exact(Item, Target, _PadChar, NbBytes) when NbBytes > Target ->
    binary:part(Item, 0, Target);
exact(Item, Target, 0, NbBytes) ->
    MissingBits = (Target - NbBytes) * 8,
    <<Item/binary, 0:MissingBits>>;
exact(Item, Target, PadChar, NbBytes) ->
    Tmp = binary_to_list(Item),
    Missing = (Target - NbBytes),
    Tail = repeat(PadChar, Missing),
    list_to_binary(Tmp ++ Tail).

repeat(X,N) ->
    lists:flatten(lists:duplicate(N,X)).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

pad_test_() ->
    [?_assertEqual(pad(<<"A">>, " "), <<"A ">>),
     ?_assertEqual(pad(<<"AB">>, " "), <<"AB">>),
     ?_assertEqual(pad(<<"ABC">>, " "), <<"ABC ">>),
     ?_assertEqual(pad(<<"ABCD">>, " "), <<"ABCD">>)].

limit_test_() ->
    [?_assertEqual(limit(<<"ABC">>, 1), <<"A">>),
     ?_assertEqual(limit(<<"ABC">>, 2), <<"AB">>),
     ?_assertEqual(limit(<<"ABC">>, 3), <<"ABC">>),
     ?_assertEqual(limit(<<"ABC">>, 4), <<"ABC">>) ].

exact_test_() ->
    [ ?_assertEqual(exact(<<"AB">>, 3, 0), <<"AB", 0>>)
    , ?_assertEqual(exact(<<"AB">>, 3, " "), <<"AB ">>)
    , ?_assertEqual(exact(<<"AB">>, 2, " "), <<"AB">>)
    , ?_assertEqual(exact(<<"AB">>, 1, " "), <<"A">>)
    ].

trim_test_() ->
    [ ?_assertEqual(trim(<<" ">>), <<"">>)
    , ?_assertEqual(trim(<<"A ">>), <<"A">>)
    , ?_assertEqual(trim(<<"AB">>), <<"AB">>)
    , ?_assertEqual(trim(<<"ABC ">>), <<"ABC">>)
    , ?_assertEqual(trim(<<"ABCD">>), <<"ABCD">>)
    ].
