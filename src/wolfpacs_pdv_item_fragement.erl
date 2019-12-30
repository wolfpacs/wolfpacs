%%%-------------------------------------------------------------------
%% @doc WolfPACS's Protocol Data Value (PDV) Framgment Item
%%
%% Ref: pg 200
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_pdv_item_fragement).
-export([encode/3,
	 decode/1]).

%%-------------------------------------------------------------------
%% @doc Encodes ad PDV Fragment Item.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(IsCommand :: boolean(), IsLast :: boolean(), PDVData :: binary()) -> binary().
encode(false, false, Data) ->
    <<0:6, 0:1, 0:1, Data/binary>>;
encode(false, true, Data) ->
    <<0:6, 0:1, 1:1, Data/binary>>;
encode(true, false, Data) ->
    <<0:6, 1:1, 0:1, Data/binary>>;
encode(true, true, Data) ->
    <<0:6, 1:1, 1:1, Data/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes ad PDV Fragment Item.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(binary()) -> {ok, IsCommand :: boolean(), IsLast :: boolean(), PDVData :: binary()} | {error, binary()}.
decode(<<0:6, B1:1, B0:1, Data/binary>>) ->
    {ok, B1==1, B0==1, Data};
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test_() ->
    Value = <<1, 2, 3, 4, 5>>,
    Encoded00 = encode(false, false, Value),
    Encoded01 = encode(false, true, Value),
    Encoded10 = encode(true, false, Value),
    Encoded11 = encode(true, true, Value),

    Correct00 = {ok, false, false, Value},
    Correct01 = {ok, false, true, Value},
    Correct10 = {ok, true, false, Value},
    Correct11 = {ok, true, true, Value},

    [?_assertEqual(decode(Encoded00), Correct00),
     ?_assertEqual(decode(Encoded01), Correct01),
     ?_assertEqual(decode(Encoded10), Correct10),
     ?_assertEqual(decode(Encoded11), Correct11)].
