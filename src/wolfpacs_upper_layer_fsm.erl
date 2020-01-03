-module(wolfpacs_upper_layer_fsm).
-behaviour(gen_statem).
-include("wolfpacs_types.hrl").

-export([start/1,
	 stop/1,
	 pdu/3]).
-export([init/1,
	 callback_mode/0,
	 terminate/3,
	 code_change/4]).
-export([idle/3]).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

-spec start(pid()) -> {ok, pid()}.
start(UpperLayer) ->
    gen_statem:start(?MODULE, UpperLayer, []).

-spec stop(pid()) -> ok.
stop(FSM) ->
    gen_statem:stop(FSM).

-spec pdu(pid(), pos_integer(), binary()) -> ok.
pdu(FSM, PDUType, PDU) ->
    gen_statem:cast(FSM, {pdu, PDUType, PDU}).

%%-----------------------------------------------------------------------------
%% Behaviour Callbacks
%%-----------------------------------------------------------------------------

-record(wolfpacs_upper_layer_fsm_data, {upper_layer :: pid()}).

init(UpperLayer) ->
    State = idle,
    Data = #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer},
    {ok, State, Data}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%-----------------------------------------------------------------------------
%% States
%%-----------------------------------------------------------------------------

idle(cast, {pdu, 1, PDU}, Data) ->
    MaybeAssociateRQ = wolfpacs_associate_rq:decode(PDU),
    handle_associate_rq(MaybeAssociateRQ, Data),
    {keep_state, Data, []};

idle(cast, {pdu, 4, PDU}, Data) ->
    MaybePDataTF = wolfpacs_p_data_tf:decode(PDU),
    handle_p_data_tf(MaybePDataTF, Data);

idle(cast, {pdu, 5, PDU}, Data) ->
    MaybeReleaseRQ = wolfpacs_release_rq:decode(PDU),
    handle_release_rq(MaybeReleaseRQ, Data);

idle(cast, {pdu, 7, PDU}, Data) ->
    MaybeAbort = wolfpacs_abort:decode(PDU),
    handle_abort(MaybeAbort, Data);

idle(cast, {pdu, N, _PDU}, Data) ->
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,
    UpperLayer ! {unknown_pdu, N},
    lager:warning("unknown pdu type ~p", [N]),
    {keep_state, Data, []};

idle(cast, What, Data) ->
    lager:warning("cast what ~p", [What]),
    {keep_state, Data, []};

idle(Type, What, Data) ->
    lager:warning("unhandle ~p ~p", [Type, What]),
    {keep_state, Data, []}.

%%-----------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

handle_associate_rq({error, _}, Data) ->
    lager:warning("associate rq decode error"),
    {keep_state, Data, []};

handle_associate_rq(AssociateRQ, Data) ->
    MaxPDUSize = 65536, %% TODO: Refactor this magic value
    TransferSyntax = wolfpacs_transfer_syntax:implicit_vr_little_endian(),

    {ok,
     CalledAE, CallingAE, R,
     Contexts,
     _MaxSize, Class, VersionName,
     _Rest} = AssociateRQ,

    %% TODO: We need to extract the correct PrCIDs here,
    %% that correspond to what we support
    [{PrCID, _AbstractSyntax, _TransferSyntexes}|_] = Contexts,

    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,

    AssociateAC = wolfpacs_associate_ac:encode(CalledAE, CallingAE, R,
					       PrCID, TransferSyntax,
					       MaxPDUSize, Class, VersionName),

    UpperLayer ! {send_response, AssociateAC},
    {keep_state, Data, []}.

handle_abort({error, _}, Data) ->
    {keep_state, Data, []};
handle_abort({ok, Source, Reason, _}, Data) ->
    lager:debug("[upper_layer_fsm] received abort"),
    {keep_state, Data, []}.

handle_p_data_tf({error, _}, Data) ->
    {keep_state, Data, []};

handle_p_data_tf({ok, PDataTF, _Rest}, Data) ->
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,

    lager:warning("p data tf ok, ~p", [PDataTF]),
    [{pdv_item, 1, true, true, Raw}] = PDataTF,
    EchoRQ = wolfpacs_dimse_protocol:decode(Raw),
    EchoResp = wolfpacs_c_echo_scp:react(EchoRQ),

    PDVItem = #pdv_item{pr_cid=1,
			is_command=true,
			is_last=true,
			pdv_data=EchoResp},

    EchoRespPDataTF = wolfpacs_p_data_tf:encode([PDVItem]),

    UpperLayer ! {send_response, EchoRespPDataTF},

    {keep_state, Data, []}.

handle_release_rq({error, _}, Data) ->
    {keep_state, Data, []};
handle_release_rq({ok, R, _}, Data) ->
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,
    ReleaseRP = wolfpacs_release_rp:encode(R),
    UpperLayer ! {send_response, ReleaseRP},
    {keep_state, Data, []}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

minimal_test() ->
    {ok, FMS} = start(self()),
    pdu(FMS, 12345, <<1, 2, 3, 4, 5>>),
    Success = receive
		  {unknown_pdu, _N} ->
		      true;
		  _ ->
		      false
	      after 1000 ->
		      false
	      end,
    ?assert(Success).
