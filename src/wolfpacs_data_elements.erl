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
%% @doc Data Elements.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_data_elements).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-spec encode(flow(), strategy(), map() | list()) -> binary().
encode(Flow, Strategy, Info) when is_map(Info) ->
    encode(Flow, Strategy, maps:to_list(Info));
encode(Flow, Strategy, UnsortedElements) ->
    Elements = lists:sort(UnsortedElements),
    Extra = priv_extract_extra(Elements, #{}),
    encode(Flow, Strategy, Extra, Elements, <<>>).

-spec decode(flow(), strategy(), binary()) -> {ok, map(), binary()} | error.
decode(Flow, Strategy, Data) ->
    decode(Flow, Strategy, Data, [], #{}).

%%==============================================================================
%% Private Encoders
%%==============================================================================

encode(_Flow, _, _, [], Acc) ->
    Acc;
encode(Flow, Strategy, Extra, [{{G, E}, Data}|Rest], Acc) ->
    VR = wolfpacs_group_elements:vr(G, E),
    Encoded = wolfpacs_data_element:encode(Flow, Strategy, G, E, VR, Data, Extra),
    encode(Flow, Strategy, Extra, Rest, <<Acc/binary, Encoded/binary>>);
encode(Flow, Strategy, Extra, [{{G, E, VR}, Data}|Rest], Acc) ->
    Encoded = wolfpacs_data_element:encode(Flow, Strategy, G, E, VR, Data, Extra),
    encode(Flow, Strategy, Extra, Rest, <<Acc/binary, Encoded/binary>>).

-spec decode(flow(), strategy(), binary(), list(), map()) -> {ok, map(), binary()} | error.
decode(_Flow, _, <<>>, Acc, _) ->
    {ok, maps:from_list(Acc), <<>>};
decode(Flow, Strategy, Data, Acc, Extra) ->
    case wolfpacs_data_element:decode(Flow, Strategy, Data, Extra) of
	{ok, Res, Rest} ->
	    decode(Flow, Strategy, Rest, [Res|Acc], priv_extract_extra([Res], Extra));
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "failed to decode data element"),
	    error
    end.

%%
%
%%

-define(BITS_ALLOCATED, {16#0028,16#0100}).
-define(PIXEL_DATA, {16#7fe0, 16#0010}).

priv_extract_extra([], Extra) ->
    Extra;
priv_extract_extra([{?BITS_ALLOCATED, 8}|Rest], Extra) ->
    priv_extract_extra(Rest, Extra#{?PIXEL_DATA => "OB"});
priv_extract_extra([{?BITS_ALLOCATED, 16}|Rest], Extra) ->
    priv_extract_extra(Rest, Extra#{?PIXEL_DATA => "OW"});
priv_extract_extra([_|Rest], Extra) ->
    priv_extract_extra(Rest, Extra).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

-define(CMD, 16#0000).
-define(UID, 16#0002).
-define(FLD, 16#0100).

-define(RQID, 16#0110).
-define(RPID, 16#0120).
-define(SET, 16#0800).
-define(STU, 16#0900).

encode_decode_common(Strategy) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    UID = <<"1.2.3.4">>,
    Items = #{{?CMD, ?UID} => UID,
	      {?CMD, ?FLD} => 16#8030,
	      {?CMD, ?RPID} => ?RQID,
	      {?CMD, ?SET} => 16#0101,
	      {?CMD, ?STU} => 16#0000},

    Encoded0 = encode(Flow, Strategy, Items),

    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect3 = <<1, 2, 3, 4, 5>>,

    [ ?_assertEqual(decode(Flow, Strategy, Encoded0), {ok, Items, <<>>})
    , ?_assertEqual(decode(Flow, Strategy, Incorrect1), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect2), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect3), error)
    ].


encode_decode_explicit_little_test_() ->
    Strategy = {explicit, little},
    encode_decode_common(Strategy).

encode_decode_explicit_big_test_() ->
    Strategy = {explicit, big},
    encode_decode_common(Strategy).

encode_decode_implicit_little_test_() ->
    Strategy = {implicit, little},
    encode_decode_common(Strategy).

encode_decode_implicit_big_test_() ->
    Strategy = {implicit, big},
    encode_decode_common(Strategy).
