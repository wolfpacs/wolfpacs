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
%% @doc WolfPACS's Protocol Data Value (PDV) Item
%%
%% Ref: pg 200
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_pdv_item).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

%%-------------------------------------------------------------------
%% @doc Encodes ad PDV Item.
%%
%% @see wolfpacs_pdv_item_fragment:encode/3
%% @end
%%-------------------------------------------------------------------
-spec encode(flow(), #pdv_item{}) -> binary().
encode(_Flow, PDVItem) ->
    #pdv_item{pr_cid=PrCID,
	      is_last=IsLast,
	      is_command=IsCommand,
	      pdv_data=PDVData} = PDVItem,
    Fragment = wolfpacs_pdv_item_fragement:encode(IsLast,
						  IsCommand,
						  PDVData),
    Data = <<PrCID, Fragment/binary>>,
    NbBytes = byte_size(Data),
    <<NbBytes:32, Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes ad PDV Item.
%%
%% @see wolfpacs_pdv_item_fragment:decode/1
%% @end
%%-------------------------------------------------------------------
-spec decode(flow(), binary()) -> {ok, #pdv_item{}, Rest :: binary()} | error.
decode(Flow, <<Length:32, Data/binary>>) ->
    case wolfpacs_utils:split(Data, Length) of
	{ok, <<PrCID, FragmentData/binary>>, Rest} ->
	    case wolfpacs_pdv_item_fragement:decode(FragmentData) of
		{ok, IsLast, IsCommand, PDVData} ->
		    PDVItem = #pdv_item{pr_cid=PrCID,
					is_last=IsLast,
					is_command=IsCommand,
					pdv_data=PDVData},
		    {ok, PDVItem, Rest};
		_ ->
		    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode fragement"),
		    error
	    end;
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "unable to split"),
	    error
    end;
decode(Flow, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "incorrect header"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    PDVData = <<1, 2, 3, 4, 5>>,
    PDVItem = #pdv_item{pr_cid=42,
			is_last=false,
			is_command=true,
			pdv_data=PDVData},

    Encoded0 = encode(no_flow, PDVItem),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4>>,
    Incorrect3 = <<>>,

    [ ?_assertEqual(decode(no_flow, Encoded0), {ok, PDVItem, <<>>})
    , ?_assertEqual(decode(no_flow, Encoded1), {ok, PDVItem, <<42>>})
    , ?_assertEqual(decode(no_flow, Incorrect0), error)
    , ?_assertEqual(decode(no_flow, Incorrect1), error)
    , ?_assertEqual(decode(no_flow, Incorrect2), error)
    , ?_assertEqual(decode(no_flow, Incorrect3), error)
    ].
