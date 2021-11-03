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
