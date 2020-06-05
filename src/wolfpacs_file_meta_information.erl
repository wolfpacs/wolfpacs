%%%-------------------------------------------------------------------
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
-spec encode(pid(), strategy(), map()) -> binary().
encode(Flow, Strategy, Info) ->
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
-spec decode(pid(), strategy(), binary()) -> {ok, map(), binary()} | {error, binary(), list(string())}.
decode(Flow, Strategy, <<_:1024, "DICM", Data/binary>>) ->
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
