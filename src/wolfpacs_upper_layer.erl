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
-export([start_link/4,
	 responde/2]).

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

start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

responde(UpperLayer, Payload) ->
    gen_server:cast(UpperLayer, {responde, Payload}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {socket :: ranch_transport:socket(),
		transport :: module(),
		fsm :: pid(),
		maxPDU = 65536 :: pos_integer(),
		data = <<>> :: binary(),
		associated = false :: boolean()
	       }).

%% @hidden
init([Side, Socket, Transport, _Opts = []]) ->
    {ok, FSM} = wolfpacs_upper_layer_fsm:start(self(), Side),
    Transport:setopts(Socket, [{active, true}]),
    {ok, #state{socket=Socket, transport=Transport, fsm=FSM}}.

%% @hidden
handle_call(What, _From, State) ->
    _ = lager:warning("unhandle call ~p", [What]),
    {reply, {error, What}, State}.

%% @hidden
handle_cast({responde, Payload}, State) ->
    ok = send_response(Payload, State),
    {noreply, State};

handle_cast(What, State) ->
    _ = lager:warning("unhandle cast ~p", [What]),
    {noreply, State}.

%% @hidden
handle_info({tcp_closed, _Port}, State) ->
    {stop, normal, State};

handle_info({tcp, _Port, DataNew}, State=#state{data=DataOld}) ->
    Data = <<DataOld/binary, DataNew/binary>>,
    handle_new_data(State#state{data=Data});

handle_info({handshake, _Name, _, _, _}, State) ->
    {noreply, State};

handle_info({send_response, Payload}, State) ->
    send_response(Payload, State),
    {noreply, State};

handle_info(What, State) ->
    _ = lager:warning("[UpperLayer] Unhandle ~p", [What]),
    {noreply, State}.

%% @hidden
terminate(_Reason, #state{fsm=FSM}) ->
    wolfpacs_upper_layer_fsm:stop(FSM),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

handle_new_data(State=#state{data=Data, fsm=FSM}) ->
    case protocol_data_unit_complete(Data) of
	{ok, PDUType, PDU, Rest} ->
	    wolfpacs_upper_layer_fsm:pdu(FSM, PDUType, PDU),
	    handle_new_data(State#state{data=Rest});
	_ ->
	    %% PDU not finished
	    {noreply, State#state{data=Data}}
    end.

protocol_data_unit_complete(Data = <<PDUType, _, PDUSize:32, _/binary>>) ->
    case wolfpacs_utils:split(Data, PDUSize + 6) of
	{ok, PDU, Rest} ->
	    {ok, PDUType, PDU, Rest};
	error ->
	    error
    end;
protocol_data_unit_complete(_) ->
    error.

send_response(Payload, #state{socket=Socket, transport=Transport}) ->
    Transport:send(Socket, Payload).

%%------------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

protocol_data_unit_complete_test_() ->
    [ ?_assertEqual(protocol_data_unit_complete(<<>>), error)
    , ?_assertEqual(protocol_data_unit_complete(<<1, 5, 2:32, 42, 43, 44, 45, 46>>), {ok, 1, <<1, 5, 2:32, 42, 43>>, <<44, 45, 46>>})].
