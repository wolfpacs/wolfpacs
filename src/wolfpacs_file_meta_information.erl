 %%%-------------------------------------------------------------------
%% @doc File Meta Information.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_file_meta_information).
-export([encode/2,
	 decode/2]).
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
-spec encode(strategy(), map()) -> binary().
encode(Strategy, Info) ->
    Data = wolfpacs_data_elements:encode(Strategy, Info),

    NbBytes = byte_size(Data),
    GroupLength = wolfpacs_data_element:encode(Strategy, 2, 0, "UL", NbBytes),

    <<0:1024,
      "DICM",
      GroupLength/binary,
      Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Decode File Meta Information.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(strategy(), binary()) -> {ok, map(), binary()} | {error, binary(), list(string())}.
decode(Strategy, OrgData = <<_:1024, "DICM", Data/binary>>) ->
    case wolfpacs_data_element:decode(Strategy, Data) of
	{error, _, Msg} ->
	    {error, OrgData, [?ERROR_DECODE|Msg]};
	{ok,{{2, 0}, GroupLength}, Rest} ->
	    case wolfpacs_utils:split(Rest, GroupLength) of
		{error, _, _} ->
		    {error, OrgData, [?ERROR_SPLIT]};
		{ok, Meta, Content} ->
		    case wolfpacs_data_elements:decode(Strategy, Meta) of
			{ok, MetaMap, <<>>} ->
			    {ok, MetaMap, Content};
			{ok, MetaMap, _LostData} ->
			    {ok, MetaMap, Content};
			{error, _, Msg} ->
			    {error, OrgData, [?ERROR_META|Msg]}
		    end
	    end
    end;
decode(_Strategy, Data) ->
    {error, Data, [?ERROR_MATCH]}.

%%==============================================================================
%% Private
%%==============================================================================

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Strategy = {explicit, little},
    Info = #{{2, 1} => [0, 1],
	     {2, 2} => <<"1.2.840.10008.5.1.4.1.1.2">>},
    Encoded0 = encode(Strategy, Info),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4>>,
    Incorrect3 = <<0:1024, "DICM", 1, 2>>,
    Incorrect4 = binary:replace(Encoded0, <<"UL">>, <<"zz">>, [global]),

    [ ?_assertEqual(decode(Strategy, Encoded0), {ok, Info, <<>>})
    , ?_assertEqual(decode(Strategy, Encoded1), {ok, Info, <<42>>})
    , ?_assertEqual(decode(Strategy, Incorrect0), {error, Incorrect0, [?ERROR_MATCH]})
    , ?_assertEqual(decode(Strategy, Incorrect1), {error, Incorrect1, [?ERROR_SPLIT]})
    , ?_assertEqual(decode(Strategy, Incorrect2), {error, Incorrect2, [?ERROR_MATCH]})
    , ?_assertEqual(decode(Strategy, Incorrect3), {error, Incorrect3, [?ERROR_DECODE, "unable to handle strategy"]})
    , ?_assertEqual(decode(Strategy, Incorrect4), {error, Incorrect4, [?ERROR_DECODE, "unsupported vr", "zz"]})
   ].
