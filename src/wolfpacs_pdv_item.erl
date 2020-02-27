%%%-------------------------------------------------------------------
%% @doc WolfPACS's Protocol Data Value (PDV) Item
%%
%% Ref: pg 200
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_pdv_item).
-export([encode/1,
	 decode/1]).
-include("wolfpacs_types.hrl").

%%-------------------------------------------------------------------
%% @doc Encodes ad PDV Item.
%%
%% @see wolfpacs_pdv_item_fragment:encode/3
%% @end
%%-------------------------------------------------------------------
-spec encode(#pdv_item{}) -> binary().
encode(PDVItem) ->
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
-spec decode(binary()) -> {ok, #pdv_item{}, Rest :: binary()} | {error, binary()}.
decode(AllData = <<Length:32, Data/binary>>) ->
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
		    {error, AllData, ["unable to decode fragement"]}
	    end;
	_ ->
	    {error, AllData, ["unable to split"]}
    end;
decode(AllData) ->
    {error, AllData, ["incorrect header"]}.


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

    Encoded0 = encode(PDVItem),
    Encoded1 = <<Encoded0/binary, 42>>,

    Incorrect0 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4>>,
    Incorrect3 = <<>>,

    [ ?_assertEqual(decode(Encoded0), {ok, PDVItem, <<>>})
    , ?_assertEqual(decode(Encoded1), {ok, PDVItem, <<42>>})
    , ?_assertEqual(decode(Incorrect0), {error, Incorrect0, ["unable to split"]})
    , ?_assertEqual(decode(Incorrect1), {error, Incorrect1, ["unable to split"]})
    , ?_assertEqual(decode(Incorrect2), {error, Incorrect2, ["unable to split"]})
    , ?_assertEqual(decode(Incorrect3), {error, Incorrect3, ["incorrect header"]})
    ].
