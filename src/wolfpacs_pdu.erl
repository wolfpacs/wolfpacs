%%%-------------------------------------------------------------------
%% @doc WolfPACS's Protocol Data Unit (PDU)
%%
%%
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_pdu).
-include_lib("eunit/include/eunit.hrl").

-export([handle_pdu/1]).

handle_pdu(PDU) ->
    ItemTypes = [16#10, 16#20, 16#30, 16#40, 16#50],
    debug_items(PDU, ItemTypes),
    lager:warning("unhandle pdu: ~p", [PDU]).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

debug_items(_PDU, []) ->
    ok;
debug_items(PDU, [ItemType|ItemTypes]) ->
    debug_item(PDU, ItemType),
    debug_items(PDU, ItemTypes).

debug_item(PDU, ItemType) ->
    lager:warning("~p: ~p", [ItemType, wolfpacs_item_inspector:find(PDU, ItemType)]).

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------
