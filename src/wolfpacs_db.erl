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
%% @doc wolfpacs database layer
%% @end
%%%-------------------------------------------------------------------
-module(wolfpacs_db).

-export([install/1, uninstall/1]).
-export([add_worker/4, add_client/2]).

-record(wolfpacs_workers,
        {name :: binary(),
         host :: binary(),
         port :: pos_integer(),
         ae :: integer(),
         accepting :: boolean()}).
-record(wolfpacs_clients,
        {name :: binary(),
         ae :: binary(),
         workers :: [binary()]}).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(wolfpacs_workers,
                        [{attributes, record_info(fields, wolfpacs_workers)},
                         {index, [#wolfpacs_workers.host]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(wolfpacs_clients,
                        [{attributes, record_info(fields, wolfpacs_clients)},
                         {disc_copies, Nodes},
                         {type, bag}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

uninstall(Nodes) ->
    ok = mnesia:delete_schema(Nodes).

add_worker(Name, Host, Port, AE) ->
    add_worker(Name, Host, Port, AE, true).

add_worker(Name, Host, Port, AE, Accepting) ->
    F = fun() ->
                mnesia:write(#wolfpacs_workers{name=Name,
                                               host=Host,
                                               port=Port,
                                               ae=AE,
                                               accepting=Accepting})
        end,
    mnesia:activity(transaction, F).

add_client(Name, AE) ->
    F = fun() ->
                mnesia:write(#wolfpacs_clients{name=Name,
                                               ae=AE,
                                               workers=[]})
        end,
    mnesia:activity(transaction, F).
