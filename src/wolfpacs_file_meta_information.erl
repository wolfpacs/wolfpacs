 %%%-------------------------------------------------------------------
%% @doc File Meta Information.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_file_meta_information).
-export([encode/2,
	 decode/2]).
-include("wolfpacs_types.hrl").

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
-spec decode(strategy(), binary()) -> {ok, map(), binary()} | {error, binary()}.
decode(Strategy, OrgData = <<_:1024, "DICM", Data/binary>>) ->
    case wolfpacs_data_element:decode(Strategy, Data) of
	{error, _} ->
	    {error, OrgData};
	{ok,{{2, 0}, GroupLength}, Rest} ->
	    lager:warning("Group length ~p", [GroupLength]),
	    case wolfpacs_utils:split(Rest, GroupLength) of
		{error, _} ->
		    {error, OrgData};
		{ok, Meta, Content} ->
		    case wolfpacs_data_elements:decode(Strategy, Meta) of
			{ok, MetaMap, <<>>} ->
			    {ok, MetaMap, Content};
			_ ->
			    {ok, OrgData}
		    end
	    end;
	_ ->
	    {error, OrgData}
    end;
decode(_Strategy, Data) ->
    {error, Data}.

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

    [ ?_assertEqual(decode(Strategy, Encoded0), {ok, Info, <<>>})
    , ?_assertEqual(decode(Strategy, Encoded1), {ok, Info, <<42>>})
    , ?_assertEqual(decode(Strategy, Incorrect0), {error, Incorrect0})
    , ?_assertEqual(decode(Strategy, Incorrect1), {error, Incorrect1})
    , ?_assertEqual(decode(Strategy, Incorrect2), {error, Incorrect2})
    , ?_assertEqual(decode(Strategy, Incorrect3), {error, Incorrect3})
    ].
