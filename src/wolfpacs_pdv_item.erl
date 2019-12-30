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
	      is_command=IsCommand,
	      is_last=IsLast,
	      pdv_data=PDVData} = PDVItem,
    Fragment = wolfpacs_pdv_item_fragement:encode(IsCommand,
						  IsLast,
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
		{ok, IsCommand, IsLast, PDVData} ->
		    PDVItem = #pdv_item{pr_cid=PrCID,
					is_command=IsCommand,
					is_last=IsLast,
					pdv_data=PDVData},
		    {ok, PDVItem, Rest};
		_ ->
		    {error, AllData}
	    end;
	_ ->
	    {error, AllData}
    end.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    PDVData = <<1, 2, 3, 4, 5>>,
    PDVItem = #pdv_item{pr_cid=42,
			is_command=true,
			is_last=false,
			pdv_data=PDVData},
    Rest = <<42, 44>>,

    Encoded0 = encode(PDVItem),
    Encoded1 = <<Encoded0/binary, Rest/binary>>,

    Correct0 = {ok, PDVItem, <<>>},
    Correct1 = {ok, PDVItem, Rest},

    Incorrect = <<1,2,3,5>>,

    [?_assertEqual(decode(Encoded0), Correct0),
     ?_assertEqual(decode(Encoded1), Correct1),
     ?_assertEqual(decode(Incorrect), {error, Incorrect})].
