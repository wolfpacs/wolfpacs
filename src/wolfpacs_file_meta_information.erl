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
decode(<<_:1024, "DICM", Data/binary>>) ->
    wolfpacs_data_elements_explicit:decode(Data);
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
    Info = #{{2, 0} => 60,
	     {2, 1} => [0, 1],
	     {2, 2} => <<"1.2.840.10008.5.1.4.1.1.2">>},
    Encoded0 = encode(Info),
    Encoded1 = <<Encoded0/binary, 42>>,
    Incorrect0 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4>>,

    [ ?_assertEqual(decode(Encoded0), {ok, Info, <<>>}),
      ?_assertEqual(decode(Encoded1), {ok, Info, <<42>>}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
      ?_assertEqual(decode(Incorrect1), {error, Incorrect1}) ].
