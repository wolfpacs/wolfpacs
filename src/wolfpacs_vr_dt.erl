%%%-------------------------------------------------------------------
%% @doc Value Representation Date Time.
%%
%% A concatenated date-time character string in the format:
%%  YYYYMMDDHHMMSS.FFFFFF&ZZXX
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
%% &ZZXX is an optional suffix for offset from Coordinated Universal
%% Time (UTC), where & = "+" or "-", and ZZ = Hours and
%% XX = Minutes of offset.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_vr_dt).
-export([encode/2,
	 decode/2]).
-import(wolfpacs_vr_utils, [pad_binary/1,
			    trim_binary/1]).

-type un() :: list() | binary().

encode(_Strategy, DT) ->
    encode(DT).

decode(_Strategy, DT) ->
    decode(DT).

%%==============================================================================
%% Private
%%==============================================================================

-spec encode(un()) -> binary().
encode(DT) when is_list(DT) ->
    encode(list_to_binary(DT));
encode(DT) ->
    pad_binary(DT).

-spec decode(binary()) -> binary().
decode(<<>>) ->
    {error, <<>>, ["empty DT"]};
decode(Data) ->
    {ok, trim_binary(Data), <<>>}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
