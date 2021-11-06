%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%
%% @doc Abstract Syntax.
%%
%% Abstract Syntax concerns what information is exchanged.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_abstract_syntax).
-export([encode/1,
	 decode/1]).
-import(wolfpacs_utils, [drop_last_byte/1]).

%%-------------------------------------------------------------------
%% @doc Encodes an Abstract Syntax UID.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(AbstractSyntaxUID :: binary()) -> <<_:32,_:_*8>>.
encode(AbstractSyntaxUID) ->
    Length = byte_size(AbstractSyntaxUID),
    <<16#30,
      0,
      Length:16,
      AbstractSyntaxUID/binary>>.

%%-------------------------------------------------------------------
%% @doc Decodes an Abstract Syntax UID.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(binary()) -> {ok, UID :: binary(), Rest :: binary()} | {error, Data :: binary()}.
decode(Payload = <<16#30, _, Length:16, Data/binary>>) ->
    NbBytes = byte_size(Data),
    case Length =< NbBytes of
	true ->
	    AbstractSyntaxString = binary:part(Data, 0, Length),
	    Rest = binary:part(Data, Length, NbBytes - Length),
	    {ok, AbstractSyntaxString, Rest};
	false ->
	    {error, Payload}
    end;
decode(Data) ->
    {error, Data}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

test_encode_test_() ->
    V0 = <<"1.2.840.10008.1.1">>,
    E0 = encode(V0),
    E1 = <<E0/binary, 42>>,
    I0 = drop_last_byte(E0),
    I1 = <<1,2,3,4>>,
    [ ?_assert(decode(E0) =:= {ok, V0, <<>>}),
      ?_assert(decode(E1) =:= {ok, V0, <<42>>}),
      ?_assert(decode(I0) =:= {error, I0}),
      ?_assert(decode(I1) =:= {error, I1})].
