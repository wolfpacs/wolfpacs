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
%% @doc Value Representation Unknown.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_un).
-export([encode/3,
	 decode/3]).

-include("wolfpacs_types.hrl").

encode(Flow, _Strategy, UN) when is_binary(UN) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, UN, 0);
encode(Flow, _Strategy, UN) when is_list(UN) ->
    wolfpacs_vr_common:encode(Flow, ?MODULE, list_to_binary(UN), 0).

decode(Flow, _Strategy, UN) ->
    wolfpacs_vr_common:decode(Flow, ?MODULE, UN).
