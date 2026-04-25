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
%% @doc wolfpacs public API
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(normal, []) ->
    ok = ensure_tables_ready(),
    wolfpacs_sup:start_link().

%%====================================================================
%% Internal functions
%%====================================================================

ensure_tables_ready() ->
    %% Note: table names must match those created in wolfpacs_db:install/1.
    ExpectedTables = [wolfpacs_worker, wolfpacs_client],
    ExistingTables = mnesia:system_info(tables),
    PresentTables = [T || T <- ExpectedTables, lists:member(T, ExistingTables)],

    case PresentTables of
        [] ->
            %% Database is not installed (yet). WolfPACS currently runs without
            %% relying on persistent Mnesia tables, so we don't fail startup.
            logger:info("[WolfPACS] Mnesia tables not installed; skipping wait"),
            ok;
        _ ->
            TimeoutMs = 5000,
            case mnesia:wait_for_tables(PresentTables, TimeoutMs) of
                ok ->
                    ok;
                {timeout, BadTabs} ->
                    logger:warning("[WolfPACS] Timeout waiting for Mnesia tables: ~p", [BadTabs]),
                    ok
            end
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ensure_tables_ready_no_tables_test() ->
    %% This test asserts that startup does not crash when the Mnesia schema/tables
    %% are not installed (common in dev/test setups).
    _ = application:ensure_all_started(mnesia),
    ?assertEqual(ok, ensure_tables_ready()).

-endif.
