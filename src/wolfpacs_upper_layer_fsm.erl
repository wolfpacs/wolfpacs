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

-export([start/2,
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

-spec start(pid(), atom()) -> {ok, pid()}.
start(UpperLayer, RouteTag) ->
    gen_statem:start(?MODULE, [UpperLayer, RouteTag], []).

-spec stop(pid()) -> ok.
stop(FSM) ->
    gen_statem:stop(FSM).

-spec pdu(pid(), pos_integer(), binary()) -> ok.
pdu(FSM, PDUType, PDU) ->
    gen_statem:cast(FSM, {pdu, PDUType, PDU}).

%%=============================================================================
%% Behaviour Callbacks
%%=============================================================================

-record(wolfpacs_upper_layer_fsm_data, {flow :: flow(),
					route_tag :: atom(),
					called_ae :: binary(),
					calling_ae :: binary(),
					upper_layer :: pid(),
					context_map :: map(),
					request_uid :: binary(),
					request_id :: integer(),
					affected_uid :: binary(),
					blob :: binary()}).

init([UpperLayer, RouteTag]) ->
    State = idle,
    {ok, Flow} = wolfpacs_flow:start_link(),
    Data = #wolfpacs_upper_layer_fsm_data{flow = Flow,
					  route_tag = RouteTag,
					  called_ae = <<>>,
					  calling_ae = <<>>,
					  upper_layer = UpperLayer,
					  context_map = #{},
					  request_uid = <<>>,
					  request_id = 0,
					  affected_uid = <<>>,
					  blob = <<>>},
    {ok, State, Data}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, Data) ->
    #wolfpacs_upper_layer_fsm_data{flow = Flow} = Data,
    _ = wolfpacs_flow:stop(Flow),
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

    {ok, Allowed} = wolfpacs_authentication:authenticate(CallingAE, CalledAE),

    {ok, SupportedContexts, ContextMap} = wolfpacs_conformance:supported(Contexts, Allowed),
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,

    AssociateAC = wolfpacs_associate_ac:encode(CalledAE, CallingAE, R,
					       SupportedContexts,
					       MaxPDUSize, Class, VersionName),

    UpperLayer ! {send_response, AssociateAC},
    NewData = Data#wolfpacs_upper_layer_fsm_data{context_map=ContextMap,
						 called_ae=CalledAE,
						 calling_ae=CallingAE
						},
    {keep_state, NewData, []}.

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
    #wolfpacs_upper_layer_fsm_data{flow=Flow, upper_layer=UpperLayer} = Data,

    {ok, EchoRQ, _Rest} = wolfpacs_data_elements:decode(Flow, Strategy, Raw),
    #{{0, 16#0002} := UID, {0, 16#0110} := RQID} = EchoRQ,
    EchoResp = wolfpacs_c_echo_scp:encode(Flow, Strategy, UID, RQID),

    PDVItem = #pdv_item{pr_cid=PrCID,
			is_last=true,
			is_command=true,
			pdv_data=EchoResp},

    EchoRespPDataTF = wolfpacs_p_data_tf:encode([PDVItem]),
    wolfpacs_upper_layer:responde(UpperLayer, EchoRespPDataTF),

    {keep_state, Data, []};

handle_pdv_item({_AbstractSyntrax, _Strategy}, _, false, false, Fragment, Data) ->
    #wolfpacs_upper_layer_fsm_data{blob=OldBlob} = Data,
    NewBlob = <<OldBlob/binary, Fragment/binary>>,
    NewData = Data#wolfpacs_upper_layer_fsm_data{blob=NewBlob},
    {keep_state, NewData, []};

handle_pdv_item({_AbstractSyntrax, Strategy}, PrCID, true, false, Fragment, Data) ->
    #wolfpacs_upper_layer_fsm_data{flow=Flow,
				   route_tag=RouteTag,
				   called_ae=CalledAE,
				   calling_ae=CallingAE,
				   upper_layer=UpperLayer,
				   blob=OldBlob,
				   request_uid=UID,
				   request_id=RQID,
				   affected_uid=AffectedUID} = Data,
    NewBlob = <<OldBlob/binary, Fragment/binary>>,

    %% Very important section of the code.
    %% We have received a complete payload, NewBlob.
    %% For maximum flexibility in WolfPACS, we route
    %% The payload deeper into the framework.
    Decoded = wolfpacs_data_elements:decode(Flow, Strategy, NewBlob),
    case route_payload(Flow, RouteTag, CalledAE, CallingAE, Decoded) of
	ok ->
	    StoreResp = wolfpacs_c_store_scp:encode(Flow, Strategy, UID, RQID, AffectedUID),
	    PDVItem = #pdv_item{pr_cid=PrCID,
				is_last=true,
				is_command=true,
				pdv_data=StoreResp},
	    StoreRespPDataTF = wolfpacs_p_data_tf:encode([PDVItem]),
	    wolfpacs_upper_layer:responde(UpperLayer, StoreRespPDataTF),
	    {keep_state, Data#wolfpacs_upper_layer_fsm_data{blob = <<>>}, [hibernate]};
	_ ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "route_payload failed"),
	    {keep_state, Data#wolfpacs_upper_layer_fsm_data{blob = NewBlob}, [stop]}
    end;

handle_pdv_item({_AbstractSyntrax, Strategy}, _, true, true, Fragment, Data) ->
    #wolfpacs_upper_layer_fsm_data{flow=Flow, blob=OldBlob} = Data,
    NewBlob = <<OldBlob/binary, Fragment/binary>>,
    {ok, Info, _Rest} = wolfpacs_data_elements:decode(Flow, Strategy, NewBlob),
    #{{0, 16#0002} := UID,
      {0, 16#0110} := RQID,
      {0, 16#1000} := AffectedUID} = Info,
    NewData = Data#wolfpacs_upper_layer_fsm_data{blob = <<>>,
						 request_uid = UID,
						 request_id = RQID,
						 affected_uid = AffectedUID},
    {keep_state, NewData, []};

handle_pdv_item(Tag, PrCID, A, B, _, Data) ->
    _ = lager:warning("unhandle pdv item ~p, ~p, ~p, ~p", [Tag, PrCID, A, B]),
    {keep_state, Data, []}.

%%
%%
%%

route_payload(Flow, RouteTag, CalledAE, CallingAE, {ok, DataSet, <<>>}) ->
    wolfpacs_flow:good(Flow, ?MODULE, "pass on payload to storage"),
    StudyUID = maps:get({16#0020, 16#000d}, DataSet, missing),
    case RouteTag of
	wolfpacs_outside ->
	    wolfpacs_outside_router:route(CalledAE, CallingAE, DataSet, StudyUID),
	    wolfpacs_inside_router:remember({CalledAE, CallingAE}, StudyUID);
	wolfpacs_inside ->
	    wolfpacs_inside_router:route(StudyUID, DataSet);
	_ ->
	    lager:warning("[UpperLayerFSM] Critical error. Incorrect RouteTag: ~p", [RouteTag])
    end;
route_payload(Flow, _, _, _, _) ->
    wolfpacs_flow:failed(Flow, ?MODULE, "unable to decode payload"),
    error.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

minimal_test() ->
    {ok, FMS} = start(self(), wolfpacs_outside),
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
