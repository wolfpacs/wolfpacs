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
%% @doc File Meta Information.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_file_meta_information).
-export([encode/3,
	 decode/3]).
-include("wolfpacs_types.hrl").

-define(ERROR_DECODE, "failed to decode data element").
-define(ERROR_SPLIT, "failed to split").
-define(ERROR_META, "failed to decode meta data element").
-define(ERROR_MATCH, "failed to pattern match").

%%-------------------------------------------------------------------
%% @doc Encod File Meta Information.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(flow(), strategy(), map()) -> binary().
encode(Flow, _Strategy, Info) ->
    Strategy = {explicit, little},
    Data = wolfpacs_data_elements:encode(Flow, Strategy, Info),

    NbBytes = byte_size(Data),
    GroupLength = wolfpacs_data_element:encode(Flow, Strategy, 2, 0, "UL", NbBytes),

    <<0:1024,
      "DICM",
      GroupLength/binary,
      Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Decode File Meta Information.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(flow(), strategy(), binary()) -> {ok, map(), binary()} | error.
decode(Flow, _Strategy, <<_:1024, "DICM", Data/binary>>) ->
    Strategy = {explicit, little},
    case wolfpacs_data_element:decode(Flow, Strategy, Data) of
	{ok, {{2, 0}, GroupLength}, Rest} ->
	    case wolfpacs_utils:split(Rest, GroupLength) of
		{ok, Meta, Content} ->
		    case wolfpacs_data_elements:decode(Flow, Strategy, Meta) of
			{ok, MetaMap, _LostData} ->
			    {ok, MetaMap, Content};
			_ ->
			    wolfpacs_flow:failed(Flow, ?MODULE, ?ERROR_META),
			    error
		    end;
		_ ->
		    wolfpacs_flow:failed(Flow, ?MODULE, ?ERROR_SPLIT),
		    error
	    end;
	{ok, _, _} ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "corrupt group length"),
	    error;
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "enable to decode data element"),
	    error
    end;
decode(Flow, _Strategy, _Data) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to match"),
    error.

%%==============================================================================
%% Private
%%==============================================================================

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    Info = #{{2, 1} => [0, 1],
	     {2, 2} => <<"1.2.840.10008.5.1.4.1.1.2">>},
    Encoded0 = encode(Flow, Strategy, Info),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4>>,
    Incorrect3 = <<0:1024, "DICM", 1, 2>>,
    Incorrect4 = binary:replace(Encoded0, <<"UL">>, <<"zz">>, [global]),

    [ ?_assertEqual(decode(Flow, Strategy, Encoded0), {ok, Info, <<>>})
    , ?_assertEqual(decode(Flow, Strategy, Encoded1), {ok, Info, <<42>>})
    , ?_assertEqual(decode(Flow, Strategy, Incorrect0), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect1), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect2), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect3), error)
    , ?_assertEqual(decode(Flow, Strategy, Incorrect4), error)
   ].
