%%%-------------------------------------------------------------------
%% @doc WolfPACS's DICOM Upper Layer Protocol for TCP/IP
%%
%%
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_upper_layer).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/4]).

%% Behaviour
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(_Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Socket, Transport, Opts], []).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {socket,
		transport,
		data = <<>>
	       }).

%% @hidden
init([Socket, Transport, _Opts = []]) ->
    Transport:setopts(Socket, [{active, true}]),
    {ok, #state{socket=Socket, transport=Transport}}.

%% @hidden
handle_call(What, _From, State) ->
    lager:warning("unhandle call ~p", [What]),
    {reply, {error, What}, State}.

%% @hidden
handle_cast(What, State) ->
    lager:warning("unhandle cast ~p", [What]),
    {noreply, State}.

%% @hidden
handle_info({tcp_closed, _Port}, State) ->
    lager:debug("connection closed"),
    {stop, normal, State};

handle_info({tcp, _Port, DataNew}, State0=#state{data=DataOld}) ->
    Data = <<DataOld/binary, DataNew/binary>>,
    case protocol_data_unit_complete(Data) of
	{ok, PDU, Rest} ->
	    handle_protocol_data_unit(PDU),
	    {noreply, State0#state{data=Rest}};
	{error, Data} ->
	    {noreply, State0#state{data=Data}}
    end;

handle_info({handshake, wolfpack, _, _, _}, State) ->
    lager:debug("handshake"),
    {noreply, State};

handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------
protocol_data_unit_complete(Data = <<1, _, PDUSize:32, Payload/binary>>) ->
    FullSize = byte_size(Payload),
    case FullSize >= PDUSize of
	true ->
	    PDU = binary:part(Payload, 0, PDUSize),
	    Rest = binary:part(Payload, PDUSize, FullSize - PDUSize),
	    {ok, PDU, Rest};
	false ->
	    {error, Data}
    end;
protocol_data_unit_complete(Data) ->
    {error, Data}.

handle_protocol_data_unit(PDU) ->
    lager:warning("unhandle pdu: ~p", [PDU]).

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

protocol_data_unit_complete_test_() ->
    [ ?_assert(protocol_data_unit_complete(<<>>) =:= {error, <<>>}),
      ?_assert(protocol_data_unit_complete(<<1, 5, 2:32, 42, 43, 44, 45>>) =:= {ok, <<42, 43>>, <<44, 45>>})].
