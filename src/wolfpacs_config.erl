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
%% @doc Config.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_config).
-export([load/0,
	 load_config_file/1]).

load() ->
    spawn(fun() ->
		  timer:sleep(2000),
		  load_config_in_folder(os:getenv("WOLFPACS_DIR"))
	  end),
    ok.

load_config_in_folder(false) ->
    _ = lager:warning("[Config] No config file specified");
load_config_in_folder(Folder) ->
    Filename = filename:join(Folder, "wolfpacs.conf"),
    load_config_file(Filename).

load_config_file(Filename) ->
    load_config_content(file:consult(Filename)).

load_config_content({ok, Terms}) ->
    load_terms(Terms);
load_config_content({error, enoent}) ->
    lager:warning("[Config] Config file named but missing"),
    error;
load_config_content({error, Reason}) ->
    lager:warning("[Config] Config error ~p", [Reason]),
    error.

load_terms([]) ->
    ok;

load_terms([{client, Name, AE}|Terms]) ->
    wolfpacs_clients:add(Name, AE),
    load_terms(Terms);

load_terms([{worker, Name, Host, Port, AE}|Terms]) ->
    wolfpacs_workers:add(Name, Host, Port, AE),
    load_terms(Terms);

load_terms([{destination, Name, Host, Port, AE}|Terms]) ->
    wolfpacs_dests:add(Name, Host, Port, AE),
    load_terms(Terms);

load_terms([{client_worker, ClientName, WorkerName}|Terms]) ->
    wolfpacs_clients:assoc_worker(ClientName, WorkerName),
    load_terms(Terms);

load_terms([{client_destination, ClientName, DestinationName}|Terms]) ->
    wolfpacs_clients:assoc_dest(ClientName, DestinationName),
    load_terms(Terms);

load_terms([Item|_Terms]) ->
    _ = lager:warning("[Config] Don't understand ~p", [Item]),
    error.
