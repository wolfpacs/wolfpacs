%%%-------------------------------------------------------------------
%% @doc Value Representation Date Time.
%%
%% A concatenated date-time character string in the format:
%%  YYYYMMDDHHMMSS.FFFFFF$ZZXX
%%
%% The components of this string, from left to right, are
%%  YYYY = Year,
%%  MM = Month,
%%  DD = Day,
%%  HH = Hour (range "00" - "23"),
%%  MM = Minute (range "00" - "59"),
%%  SS = Second (range "00" - "60").
%%
%% FFFFFF = Fractional Second contains a fractional part of a second
%% as small as 1 millionth of a second (range "000000" - "999999").
%%
%% $ZZXX is an optional suffix for offset from Coordinated Universal
%% Time (UTC), where $ = "+" or "-", and ZZ = Hours and
%% XX = Minutes of offset.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_dt).
-export([encode/3,
	 decode/3]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    trim_binary/1]).

encode(Flow, _Strategy, DT) ->
    priv_encode(Flow, DT).

decode(Flow, _Strategy, DT) ->
    priv_decode(Flow, DT).

%%==============================================================================
%% Private
%%==============================================================================

priv_encode(Flow, DT) when is_list(DT) ->
    priv_encode(Flow, list_to_binary(DT));
priv_encode(_Flow, DT) ->
    pad_binary(DT).

priv_decode(Flow, <<>>) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "empty DT"),
    error;
priv_decode(_Flow, Data) ->
    {ok, trim_binary(Data), <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = <<"WolfPACS">>,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Encoded0 = encode(Flow, {explicit, little}, Data),
    {ok, Decoded0, <<>>} = decode(Flow, {explicit, little}, Encoded0),
    ?assertEqual(Data, Decoded0).
