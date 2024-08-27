%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
%%
%% @doc wolfpacs database layer
%% @end
%%%-------------------------------------------------------------------
-module(wolfpacs_db).

-export([install/1, uninstall/1]).
-export([add_worker/4, add_client/2]).
-export([worker_by_name/1, client_by_name/1]).

-include("wolfpacs_types.hrl").

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(wolfpacs_worker,
                        [{attributes, record_info(fields, wolfpacs_worker)},
                         {index, [#wolfpacs_worker.host]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(wolfpacs_client,
                        [{attributes, record_info(fields, wolfpacs_client)},
                         {disc_copies, Nodes},
                         {type, bag}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

uninstall(Nodes) ->
    ok = mnesia:delete_schema(Nodes).

add_worker(Name, Host, Port, AE) ->
    add_worker(Name, Host, Port, AE, true).

add_worker(Name, Host, Port, AE, Accepting) ->
    Worker = #wolfpacs_worker{name=Name,
                              host=Host,
                              port=Port,
                              ae=AE,
                              accepting=Accepting},
    F = fun() -> mnesia:write(Worker) end,
    mnesia:activity(transaction, F).

add_client(Name, AE) ->
    Client = #wolfpacs_client{name=Name,
                              ae=AE,
                              workers=[]},
    F = fun() -> mnesia:write(Client) end,
    mnesia:activity(transaction, F).

worker_by_name(Name) ->
    F = fun() ->
                case mnesia:read({wolfpacs_worker, Name}) of
                    [Worker] ->
                        Worker;
                    [] ->
                        undefined
                end
        end,
    mnesia:activity(transaction, F).

client_by_name(Name) ->
    F = fun() ->
                case mnesia:read({wolfpacs_client, Name}) of
                    [Client] ->
                        Client;
                    [] ->
                        undefined
                end
        end,
    mnesia:activity(transaction, F).
