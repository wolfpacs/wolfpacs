%%%-------------------------------------------------------------------
%% @doc WolfPACS's Protocol Data Value (PDV) Framgment Item
%%
%% If bit 0 is set to 1, the following fragment shall contain Message Command information.
%% If bit 0 is set to 0, the following fragment shall contain Message Data Set information.
%%
%% If bit 1 is set to 1, the following fragment shall contain the last fragment.
%% If bit 1 is set to 0, the following fragment does not contain the last fragment.
%%
%% 0 0 0 0 0 0 L? C?
%%
%% [http://dicom.nema.org/dicom/2013/output/chtml/part08/sect_E.2.html]
%% DICOM book pg 200
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
-spec encode(IsLast :: boolean(), IsCommand :: boolean(), DVData :: binary()) -> <<_:8, _:_*8>>.
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
-spec decode(binary()) -> {ok, IsLast :: boolean(), IsCommand :: boolean(), PDVData :: binary()} | {error, binary()}.
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
