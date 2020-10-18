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
%% @doc Item Inspector.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_item_inspector).
-export([find/2]).

find(Data, ItemType) ->
    find(Data, ItemType, []).

%%==============================================================================
%% Private
%%==============================================================================

find(<<ItemType, 0, Length:16, Data/binary>>, ItemType, Acc) when Length >= 1 ->
    Size = byte_size(Data),
    case Length >= 1 andalso Length =< Size of
	true ->
	    Item = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, Size - Length),
	    find(Rest, ItemType, [Item|Acc]);
	false ->
	    find(<<Length:16, Data/binary>>, ItemType, Acc)
    end;
find(<<_, Next/binary>>, ItemType, Acc) ->
    find(Next, ItemType, Acc);
find(<<>>, _, Acc) ->
    lists:reverse(Acc).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

find_test_() ->
    [
     ?_assert(find(<<>>, 1) =:= []),
     ?_assert(find(<<12, 0, 2:16, 42, 43>>, 1) =:= []),
     ?_assert(find(<<1, 0, 2:16, 42, 43>>, 1) =:= [<<42, 43>>]),
     ?_assert(find(<<12, 1, 0, 2:16, 42, 43>>, 1) =:= [<<42, 43>>]),
     ?_assert(find(<<12, 1, 0, 3:16, 42, 43>>, 1) =:= [])
    ].
