%%%-------------------------------------------------------------------
%% @doc Value Representation Sequence.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_sq).
-export([encode/3, decode/3]).

%%------------------------------------------------------------------------------
%% @doc Encode sequence.
%%
%% @end
%%------------------------------------------------------------------------------

encode(Flow, Strategy, Items) ->
    wolfpacs_flow:start_encode(Flow, ?MODULE),
    wolfpacs_vr_sq_items:encode(Flow, Strategy, Items).

%%------------------------------------------------------------------------------
%% @doc Decode sequence.
%%
%% @end
%%------------------------------------------------------------------------------

decode(Flow, Strategy, Data) ->
    wolfpacs_flow:start_decode(Flow, ?MODULE),
    wolfpacs_vr_sq_items:decode(Flow, Strategy, Data).

%%==============================================================================
%% Private
%%==============================================================================

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

book_example_one_test_() ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Strategy = {explicit, little},
    Items = [#{{16#0008, 16#0100} => <<"121327">>,
	       {16#0008, 16#0102} => <<"DCM">>,
	       {16#0008, 16#0104} => <<"Full fidelity image">>
	      },
	     #{{16#0008, 16#0100} => <<"121327">>,
	       {16#0008, 16#0102} => <<"DCM">>,
	       {16#0008, 16#0104} => <<"Full fidelity image">>
	      }
	    ],
    Encoded = encode(Flow, Strategy, Items),
    Decoded = decode(Flow, Strategy, Encoded),
    [ ?_assertEqual(Decoded, {ok, Items, <<>>})
    ].

nested_encode_common(Strategy) ->
    DataSet = [#{{8,4416} => [#{{8,4432} => <<"1.2.840.10008.5.1.4.1.1.4">>,
				{8,4437} => <<"1.2.826.0.1.3680043">>}
			     ],
		 {40,4176} => <<"128.5">>,
		 {40,4177} => <<"257">>}],
    Encoded = encode(no_flow, Strategy, DataSet),
    ?assertEqual(decode(no_flow, Strategy, Encoded), {ok, DataSet, <<>>}).

nested_encode_explicit_little_test() ->
    nested_encode_common({explicit, little}).

nested_encode_implicit_little_test() ->
    nested_encode_common({implicit, little}).

nested_encode_explicit_big_test() ->
    nested_encode_common({explicit, big}).
