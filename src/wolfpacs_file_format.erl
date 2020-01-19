%%%-------------------------------------------------------------------
%% @doc File Format.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_file_format).
-export([encode/1,
	 decode/1]).

%%-------------------------------------------------------------------
%% @doc Encodes a File Format.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(binary()) -> binary().
encode(Data) ->
    MetaInformation = wolfpacs_file_meta_information:encode(meta_info()),
    <<MetaInformation/binary, Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes a File Format.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(binary()) -> {ok, {map(), binary()}, binary()} | {error, binary()}.
decode(Data) ->
    case wolfpacs_file_meta_information:decode(Data) of
	{error, _} ->
	    {error, Data};
	{ok, Info, Rest} ->
	    {Meta, Content} = split_meta_content(Info),
	    {ok, {Meta, Content}, Rest}
    end.

%%==============================================================================
%% Private
%%==============================================================================

meta_info() ->
    #{{2, 1} => [0, 1],
      {2, 2} => <<"1.2.840.10008.5.1.4.1.1.2">>,
      {2, 3} => <<"2.16.840.1.113662.2.1.4519.41582.4105152.419990505.410523251">>,
      {2, 16#10} => <<"1.2.840.10008.1.2.1">>,
      {2, 16#12} => <<"2.16.840.1.113662.2.1.1">>,
      {2, 16#16} => <<"PHOENIXSCP">>
     }.

split_meta_content(Info) ->
    Get = fun(Group, Element, Default) ->
		  Key = {Group, Element},
		  maps:get(Key, Info, Default)
	  end,

    Meta = #{{2, 0} => Get(2, 0, 0),
	     {2, 1} => Get(2, 1, [0, 1]),
	     {2, 2} => Get(2, 2, <<>>),
	     {2, 3} => Get(2, 3, <<>>),

	     {2, 16} => Get(2, 16, <<>>),
	     {2, 18} => Get(2, 18, <<>>),

	     {2, 22} => Get(2, 22, <<>>)
	    },

    MetaKeys = [{2, 0}, {2, 1}, {2, 2}, {2, 3}, {2, 16}, {2, 18}, {2, 22}],
    Content = wolfpacs_utils:remove_keys(MetaKeys, Info),
    {Meta, Content}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Info = maps:put({2, 0}, 200, meta_info()),
    Payload = <<"this is important data">>,
    Encoded0 = encode(Payload),
    Incorrect0 = wolfpacs_utils:drop_first_byte(Encoded0),
    Incorrect1 = <<1, 2, 3, 4, 5>>,
    [ ?_assertEqual(decode(Encoded0), {ok, {Info, #{}}, Payload}),
      ?_assertEqual(decode(Incorrect0), {error, Incorrect0}),
      ?_assertEqual(decode(Incorrect1), {error, Incorrect1}) ].
