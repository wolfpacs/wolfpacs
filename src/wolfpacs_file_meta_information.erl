 %%%-------------------------------------------------------------------
%% @doc File Meta Information.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_file_meta_information).
-export([encode/1,
	 decode/1]).

%%-------------------------------------------------------------------
%% @doc Encod File Meta Information.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(map()) -> binary().
encode(Info) ->
    Data = wolfpacs_data_elements_explicit:encode_map(Info),

    NbBytes = byte_size(Data),
    GroupLength = wolfpacs_data_element_explicit:encode(2, 0, "UL", NbBytes),

    <<0:1024,
      "DICM",
      GroupLength/binary,
      Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Decode File Meta Information.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(binary()) -> {ok, map(), binary()} | {error, binary()}.
decode(OrgData = <<_:1024, "DICM", Data/binary>>) ->
    case wolfpacs_data_element_explicit:decode(Data) of
	{error, _} ->
	    {error, OrgData};
	{ok,{{2, 0}, GroupLength}, Rest} ->
	    lager:warning("Group length ~p", [GroupLength]),
	    case wolfpacs_utils:split(Rest, GroupLength) of
		{error, _} ->
		    {error, OrgData};
		{ok, Meta, Content} ->
		    case wolfpacs_data_elements_explicit:decode(Meta) of
			{ok, MetaMap, <<>>} ->
			    {ok, MetaMap, Content};
			_ ->
			    {ok, OrgData}
		    end
	    end;
	_ ->
	    {error, OrgData}
    end;
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Private
%%==============================================================================

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Info = #{{2, 1} => [0, 1],
	     {2, 2} => <<"1.2.840.10008.5.1.4.1.1.2">>},
    Encoded0 = encode(Info),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect1 = wolfpacs_utils:drop_last_byte(Encoded0),
    Incorrect2 = <<1, 2, 3, 4>>,
    Incorrect3 = <<0:1024, "DICM", 1, 2>>,

    [ ?_assertEqual(decode(Encoded0), {ok, Info, <<>>})
    , ?_assertEqual(decode(Encoded1), {ok, Info, <<42>>})
    , ?_assertEqual(decode(Incorrect0), {error, Incorrect0})
    , ?_assertEqual(decode(Incorrect1), {error, Incorrect1})
    , ?_assertEqual(decode(Incorrect2), {error, Incorrect2})
    , ?_assertEqual(decode(Incorrect3), {error, Incorrect3})
    ].
