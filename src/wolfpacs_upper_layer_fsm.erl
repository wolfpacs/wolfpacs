%%%-------------------------------------------------------------------
%% @doc Upper Layer Finite State Machine (FSM).
%%
%% Abstract Syntax concerns what information is exchanged.
%%
%% @end
%%%-------------------------------------------------------------------

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

%%=============================================================================
%% API
%%=============================================================================

-spec start(pid()) -> {ok, pid()}.
start(UpperLayer) ->
    gen_statem:start(?MODULE, UpperLayer, []).

-spec stop(pid()) -> ok.
stop(FSM) ->
    gen_statem:stop(FSM).

-spec pdu(pid(), pos_integer(), binary()) -> ok.
pdu(FSM, PDUType, PDU) ->
    gen_statem:cast(FSM, {pdu, PDUType, PDU}).

%%=============================================================================
%% Behaviour Callbacks
%%=============================================================================

-record(wolfpacs_upper_layer_fsm_data, {upper_layer :: pid(),
					context_map :: map(),
					request_uid :: binary(),
					request_id :: integer(),
					affected_uid :: binary(),
					blob :: binary()}).

init(UpperLayer) ->
    State = idle,
    Data = #wolfpacs_upper_layer_fsm_data{upper_layer = UpperLayer,
					  context_map = #{},
					  request_uid = <<>>,
					  request_id = 0,
					  affected_uid = <<>>,
					  blob = <<>>},
    {ok, State, Data}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%=============================================================================
%% States
%%=============================================================================

idle(cast, {pdu, 1, PDU}, Data) ->
    MaybeAssociateRQ = wolfpacs_associate_rq:decode(PDU),
    handle_associate_rq(MaybeAssociateRQ, Data);

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
    _ = lager:warning("unknown pdu type ~p", [N]),
    {keep_state, Data, []};

idle(cast, What, Data) ->
    _ = lager:warning("cast what ~p", [What]),
    {keep_state, Data, []};

idle(Type, What, Data) ->
    _ = lager:warning("unhandle ~p ~p", [Type, What]),
    {keep_state, Data, []}.

%%=============================================================================
%% Private
%%==============================================================================

handle_associate_rq({error, _}, Data) ->
    _ = lager:warning("associate rq decode error"),
    {keep_state, Data, []};

handle_associate_rq(AssociateRQ, Data) ->
    MaxPDUSize = 65536, %% TODO: Refactor this magic value

    {ok,
     CalledAE, CallingAE, R,
     Contexts,
     _MaxSize, Class, VersionName,
     _Rest} = AssociateRQ,

    {ok, SupportedContexts, ContextMap} = wolfpacs_conformance:supported(Contexts),
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,

    AssociateAC = wolfpacs_associate_ac:encode(CalledAE, CallingAE, R,
					       SupportedContexts,
					       MaxPDUSize, Class, VersionName),

    UpperLayer ! {send_response, AssociateAC},
    {keep_state, Data#wolfpacs_upper_layer_fsm_data{context_map=ContextMap}, []}.

handle_abort({error, _}, Data) ->
    {keep_state, Data, []};
handle_abort({ok, _Source, _Reason, _}, Data) ->
    _ = lager:debug("[upper_layer_fsm] received abort"),
    {keep_state, Data, []}.

handle_p_data_tf({error, _}, Data) ->
    {keep_state, Data, []};

handle_p_data_tf({ok, PDataTF, _Rest}, Data) ->
    #wolfpacs_upper_layer_fsm_data{context_map=ContextMap} = Data,
    [{pdv_item, PrCID, IsLast, IsCommand, Raw}] = PDataTF,

    ConformanceTag = maps:get(PrCID, ContextMap, missing),
    handle_pdv_item(ConformanceTag, PrCID, IsLast, IsCommand, Raw, Data).

handle_release_rq({error, _}, Data) ->
    {keep_state, Data, []};
handle_release_rq({ok, R, _}, Data) ->
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,
    ReleaseRP = wolfpacs_release_rp:encode(R),
    UpperLayer ! {send_response, ReleaseRP},
    {keep_state, Data, []}.

handle_pdv_item({verification, Strategy}, PrCID, _IsLast, _IsCommand, Raw, Data) ->
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,

    {ok, EchoRQ, _Rest} = wolfpacs_data_elements:decode(Strategy, Raw),
    #{{0, 16#0002} := UID, {0, 16#0110} := RQID} = EchoRQ,
    EchoResp = wolfpacs_c_echo_scp:encode(Strategy, UID, RQID),

    PDVItem = #pdv_item{pr_cid=PrCID,
			is_last=true,
			is_command=true,
			pdv_data=EchoResp},

    EchoRespPDataTF = wolfpacs_p_data_tf:encode([PDVItem]),

    UpperLayer ! {send_response, EchoRespPDataTF},

    {keep_state, Data, []};

handle_pdv_item({image_storage, _Strategy}, _, false, false, Fragment, Data) ->
    #wolfpacs_upper_layer_fsm_data{blob=OldBlob} = Data,
    NewBlob = <<OldBlob/binary, Fragment/binary>>,
    NewData = Data#wolfpacs_upper_layer_fsm_data{blob=NewBlob},
    {keep_state, NewData, []};

handle_pdv_item({image_storage, Strategy}, PrCID, true, false, Fragment, Data) ->
    #wolfpacs_upper_layer_fsm_data{blob=OldBlob,
				   request_uid=UID,
				   request_id=RQID,
				   affected_uid=AffectedUID} = Data,
    NewBlob = <<OldBlob/binary, Fragment/binary>>,
    FileData = wolfpacs_file_format:encode({explicit, little}, NewBlob),
    ok = file:write_file("abc.dcm", FileData),
    _ = lager:warning("saved abc.dcm"),


    %%
    StoreResp = wolfpacs_c_store_scp:encode(Strategy, UID, RQID, AffectedUID),

    PDVItem = #pdv_item{pr_cid=PrCID,
			is_last=true,
			is_command=true,
			pdv_data=StoreResp},

    StoreRespPDataTF = wolfpacs_p_data_tf:encode([PDVItem]),

    %%


    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,
    UpperLayer ! {send_response, StoreRespPDataTF},

    NewData = Data#wolfpacs_upper_layer_fsm_data{blob = <<>>},
    {keep_state, NewData, []};

handle_pdv_item({image_storage, Strategy}, _, true, true, Fragment, Data) ->
    #wolfpacs_upper_layer_fsm_data{blob=OldBlob} = Data,
    NewBlob = <<OldBlob/binary, Fragment/binary>>,
    {ok, Info, _Rest} = wolfpacs_data_elements:decode(Strategy, NewBlob),
    _ = lager:warning("image_storage true true ~p", [Info]),
    #{{0, 16#0002} := UID,
      {0, 16#0110} := RQID,
      {0, 16#1000} := AffectedUID} = Info,
    NewData = Data#wolfpacs_upper_layer_fsm_data{blob = <<>>,
						 request_uid = UID,
						 request_id = RQID,
						 affected_uid = AffectedUID},
    {keep_state, NewData, []};

handle_pdv_item({find_study_root_query, Strategy}, _, _A, _B, Fragment, Data) ->
    #wolfpacs_upper_layer_fsm_data{blob=OldBlob} = Data,
    NewBlob = <<OldBlob/binary, Fragment/binary>>,
    {ok, Info, _Rest} = wolfpacs_data_elements:decode(Strategy, NewBlob),
    _ = lager:warning("find_study_root_query ~p", [Info]),
    {keep_state, Data, []};

handle_pdv_item(Tag, PrCID, A, B, _, Data) ->
    _ = lager:warning("unhandle pdv item ~p, ~p, ~p, ~p", [Tag, PrCID, A, B]),
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
