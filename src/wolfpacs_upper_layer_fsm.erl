%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
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
-export([idle/3,
	 abort/3]).

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
    [state_functions, state_enter].

terminate(_Reason, _State, Data) ->
    #wolfpacs_upper_layer_fsm_data{flow = Flow} = Data,
    _ = wolfpacs_flow:stop(Flow),
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%=============================================================================
%% States
%%=============================================================================

%%-------------------------------------------------------------------
%% @doc Idle state.
%%
%% @end
%%-------------------------------------------------------------------

idle(enter, _Prev, Data) ->
    {keep_state, Data, []};

idle(cast, {pdu, 1, PDU}, Data) ->
    #wolfpacs_upper_layer_fsm_data{flow = Flow} = Data,
    MaybeAssociateRQ = wolfpacs_associate_rq:decode(Flow, PDU),
    handle_associate_rq(MaybeAssociateRQ, Data);

idle(cast, {pdu, 4, PDU}, Data) ->
    #wolfpacs_upper_layer_fsm_data{flow = Flow} = Data,
    MaybePDataTF = wolfpacs_p_data_tf:decode(Flow, PDU),
    handle_p_data_tf(MaybePDataTF, Data);

idle(cast, {pdu, 5, PDU}, Data) ->
    MaybeReleaseRQ = wolfpacs_release_rq:decode(PDU),
    handle_release_rq(MaybeReleaseRQ, Data);

idle(cast, {pdu, 7, PDU}, Data) ->
    #wolfpacs_upper_layer_fsm_data{flow = Flow} = Data,
    MaybeAbort = wolfpacs_abort:decode(Flow, PDU),
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

%%-------------------------------------------------------------------
%% @doc Abort state.
%%
%% @end
%%-------------------------------------------------------------------

abort(enter, _Prev, Data) ->
    _ = lager:warning("[UpperLayer] [Abort] Send abort"),
    #wolfpacs_upper_layer_fsm_data{flow = Flow, upper_layer=UpperLayer} = Data,
    Abort = wolfpacs_abort:encode(Flow, 2, 0),
    wolfpacs_upper_layer:responde(UpperLayer, Abort),
    {keep_state, Data, [{timeout, 500, close}]};

abort(timeout, close, Data) ->
    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer} = Data,
    _ = wolfpacs_upper_layer:stop(UpperLayer),
    {keep_state, Data, []};

abort(A, B, Data) ->
    _ = lager:warning("[UpperLayer] [Abort] Received ~p ~p", [A, B]),
    {keep_state, Data, []}.

%%=============================================================================
%% Private
%%==============================================================================

handle_associate_rq(error, Data) ->
    _ = lager:warning("associate rq decode error"),
    {keep_state, Data, []};

handle_associate_rq(AssociateRQ, Data) ->
    MaxPDUSize = 65536, %% TODO: Refactor this magic value

    {ok,
     CalledAE, CallingAE, R,
     Contexts,
     _MaxSize, Class, VersionName,
     _Rest} = AssociateRQ,

    #wolfpacs_upper_layer_fsm_data{upper_layer=UpperLayer, route_tag=RouteTag} = Data,

    {ok, Allowed} = wolfpacs_route_logic:allow(CalledAE, RouteTag),

    {ok, SupportedContexts, ContextMap} = wolfpacs_conformance:supported(Contexts, Allowed),

    AssociateAC = wolfpacs_associate_ac:encode(CalledAE, CallingAE, R,
					       SupportedContexts,
					       MaxPDUSize, Class, VersionName),

    UpperLayer ! {send_response, AssociateAC},
    NewData = Data#wolfpacs_upper_layer_fsm_data{context_map=ContextMap,
						 called_ae=CalledAE,
						 calling_ae=CallingAE
						},
    {keep_state, NewData, []}.

handle_abort(error, Data) ->
    {keep_state, Data, []};
handle_abort({ok, _Source, _Reason, _}, Data) ->
    _ = lager:debug("[upper_layer_fsm] received abort"),
    {keep_state, Data, []}.

handle_p_data_tf(error, Data) ->
    {keep_state, Data, []};

handle_p_data_tf({ok, PDataTF, _Rest}, Data) ->
    #wolfpacs_upper_layer_fsm_data{context_map=ContextMap} = Data,
    [{pdv_item, PrCID, IsLast, IsCommand, Raw}] = PDataTF,

    ConformanceTag = maps:get(PrCID, ContextMap, missing),
    handle_pdv_item(ConformanceTag, PrCID, IsLast, IsCommand, Raw, Data).

handle_release_rq(error, Data) ->
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

    EchoRespPDataTF = wolfpacs_p_data_tf:encode(Flow, [PDVItem]),
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

    %% We have received the "last" fragment. It is time to decode the dataset

    NewBlob = <<OldBlob/binary, Fragment/binary>>,

    case wolfpacs_data_elements:decode(Flow, Strategy, NewBlob) of
	{ok, DataSet, Rest} ->

	    %% Very important section of the code.
	    %% We have received a complete payload, NewBlob.
	    %% For maximum flexibility in WolfPACS, we route
	    %% The payload deeper into the framework.

	    case route_payload(Flow, RouteTag, CalledAE, CallingAE, DataSet) of
		ok ->
		    StoreResp = wolfpacs_c_store_scp:encode(Flow, Strategy, UID, RQID, AffectedUID),
		    PDVItem = #pdv_item{pr_cid=PrCID,
					is_last=true,
					is_command=true,
					pdv_data=StoreResp},
		    StoreRespPDataTF = wolfpacs_p_data_tf:encode(Flow, [PDVItem]),
		    wolfpacs_upper_layer:responde(UpperLayer, StoreRespPDataTF),
		    {keep_state, Data#wolfpacs_upper_layer_fsm_data{blob = Rest}, [hibernate]};
		_ ->
		    %% As we couldn't route the blob, we need to signal to the
		    %% client that we haven't taken care of the payload.

		    wolfpacs_flow:failed(Flow, ?MODULE, "Failed to route payload"),
		    {next_state, abort, Data#wolfpacs_upper_layer_fsm_data{blob = NewBlob}}
	    end;
	_ ->

	    %% Unable to decode the dataset. We need to send an abort
	    %% to the clint

	    wolfpacs_flow:failed(Flow, ?MODULE, "Failed to decode payload"),
	    {next_state, abort, Data#wolfpacs_upper_layer_fsm_data{blob = NewBlob}}
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

route_payload(Flow, RouteTag, CalledAE, CallingAE, DataSet) ->
    wolfpacs_flow:good(Flow, ?MODULE, "pass on payload to storage"),
    ImageType = maps:get({16#0008, 16#0008}, DataSet, missing),
    StudyUID = maps:get({16#0020, 16#000d}, DataSet, missing),
    SeriesUID = maps:get({16#0020, 16#000e}, DataSet, missing),
    wolfpacs_router_insight:note(RouteTag, CalledAE, CallingAE, ImageType, StudyUID, SeriesUID),
    wolfpacs_storage:store(DataSet),

    case RouteTag of
	wolfpacs_outside ->
	    outside_route(CalledAE, StudyUID, DataSet);
	wolfpacs_inside ->
	    inside_route(StudyUID, DataSet);
	_ ->
	    lager:warning("[UpperLayerFSM] Critical error. Incorrect RouteTag: ~p", [RouteTag])
    end.

%%==============================================================================
%% Private
%%==============================================================================

outside_route(CalledAE, StudyUID, DataSet) ->
    case wolfpacs_route_logic:pick_worker(CalledAE, StudyUID) of
	{ok, Remote} ->
	    wolfpacs_sender_pool:send(Remote, DataSet);
	_ ->
	    _ = lager:warning("[UpperLayerFSM] Unable to route client AE: ~p", [CalledAE])
    end.

inside_route(StudyUID, DataSet) ->
    case wolfpacs_route_logic:pick_destination(StudyUID) of
	{ok, Remote} ->
	    wolfpacs_sender_pool:send(Remote, DataSet);
	_ ->
	    _ = lager:warning("[UpperLayerFSM] Unable to route study uid: ~p", [StudyUID])
    end.


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
