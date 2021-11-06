%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
