%%%-------------------------------------------------------------------
%% @doc Sender
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_sender).
-behaviour(gen_statem).

-include("transfer_syntax.hrl").
-include("wolfpacs_types.hrl").

-define(SOP_CLASS_UID, {16#0008, 16#0016}).
-define(SOP_INSTANCE_UID, {16#0008, 16#0018}).

-export([start_link/4,
	 stop/1,
	 send/2]).
-export([init/1,
	 callback_mode/0,
	 terminate/3,
	 code_change/4]).
-export([idle/3,
	 associate/3,
	 send_data/3,
	 release/3,
	 finish/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Host, Port, CalledAE, CallingAE) ->
    gen_statem:start_link(?MODULE, [Host, Port, CalledAE, CallingAE], []).

stop(Sender) ->
    gen_statem:stop(Sender).

send(Sender, DataSet) ->
    ClassUID = maps:get(?SOP_CLASS_UID, DataSet, missing),
    InstanceUID = maps:get(?SOP_INSTANCE_UID, DataSet, missing),
    send_with_class_and_instance_uid(Sender, DataSet, ClassUID, InstanceUID).

send_with_class_and_instance_uid(_Sender, _DataSet, missing, _) ->
    {error, sop_class_uid_missing};
send_with_class_and_instance_uid(_Sender, _DataSet, _ClassUID, missing) ->
    {error, sop_instance_uid_missing};
send_with_class_and_instance_uid(Sender, DataSet, ClassUID, InstanceUID) ->
    gen_statem:call(Sender, {send, DataSet, ClassUID, InstanceUID}).

%%=============================================================================
%% Behaviour Callbacks
%%=============================================================================

-record(sender_data, {flow :: flow(),
		      host :: any(),
		      port :: any(),
		      called :: any(),
		      calling :: any(),
		      sock :: any(),
		      from :: any(),
		      dataset :: map(),
		      abstract_syntax :: binary(),
		      instance_uid :: binary(),
		      data :: binary(),
		      maxpdu :: non_neg_integer(),
		      chunks :: list(binary())}).

%% @hidden
init([Host, Port, CalledAE, CallingAE]) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    SenderData = #sender_data{flow = Flow,
			      host = Host,
			      port = Port,
			      called = CalledAE,
			      calling = CallingAE,
			      sock = none,
			      from = none,
			      dataset = #{},
			      abstract_syntax = <<>>,
			      instance_uid = <<>>,
			      data = <<>>,
			      maxpdu = 0,
			      chunks = []},
    {ok, idle, SenderData}.

%% @hidden
callback_mode() ->
    [state_functions, state_enter].

%% @hidden
terminate(_, _, #sender_data{flow = Flow}) ->
    _ = wolfpacs_flow:stop(Flow),
    void.

%% @hidden
code_change(_Vsn, State, SenderData, _Extra) ->
    {ok, State, SenderData}.

%%=============================================================================
%% States
%%=============================================================================

%%-------------------------------------------------------------------
%% @doc Idle state.
%%
%% The idle state is waiting for a send call from another module.
%% Will setup the tcp/ip connection to the remote host.
%%
%% @end
%%-------------------------------------------------------------------

idle(enter, _Prev, SenderData) ->
    _ = lager:debug("[Sender] [Idel] Enter"),
    {keep_state, SenderData, []};
idle({call, From}, {send, DataSet, AbstractSyntax, InstanceUID}, SenderData) ->
    _ = lager:debug("[Sender] [Idle] Connect over tcp/ip"),
    #sender_data{flow = Flow, host = Host, port = Port} = SenderData,
    wolfpacs_flow:reset(Flow),
    case gen_tcp:connect(Host, int(Port), [binary, {active, true}]) of
	{ok, Sock} ->
	    NewSenderData = SenderData#sender_data{from = From,
						   sock = Sock,
						   dataset = DataSet,
						   abstract_syntax = AbstractSyntax,
						   instance_uid = InstanceUID
						  },
	    {next_state, associate, NewSenderData};
	{error, Error} ->
	    _ = lager:warning("[Sender] [Idle] Unable to connect to ~p: ~p", [Host, Error]),
	    {keep_state, SenderData, [{reply, From, {error, Error}}]}
    end;
idle(A, B, SenderData) ->
    _ = lager:warning("[Sender] [Idel] Unknown message ~p ~p", [A, B]),
    {keep_state, SenderData, []}.

%%-------------------------------------------------------------------
%% @doc Associate state.
%%
%% Will send the Associate Requests and wait for reply.
%%
%% @end
%%-------------------------------------------------------------------

associate(enter, _Prev, SenderData) ->
    _ = lager:debug("[Sender] [Associate] Send Associate RQ"),
    #sender_data{flow = Flow,
		 sock = Sock,
		 called = CalledAE,
		 calling = CallingAE,
		 abstract_syntax = AbstractSyntax} = SenderData,

    Contexts = [{1, AbstractSyntax, [?EXPLICIT_LITTLE_ENDIAN]}],
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"WolfPACS_000">>,

    AssociateRQ = wolfpacs_associate_rq:encode(Flow,
					       CalledAE,
					       CallingAE,
					       Contexts,
					       MaxPDUSize,
					       Class,
					       VersionName),
    ok = gen_tcp:send(Sock, AssociateRQ),

    {keep_state, SenderData, []};

associate(info, {tcp, _Port, DataNew}, SenderData) ->
    #sender_data{data = DataOld} = SenderData,
    Data = <<DataOld/binary, DataNew/binary>>,
    case wolfpacs_associate_ac:decode(Data) of
	{ok, _CalledAE, _CallingAE, _R, Contexts, MaxPDU, _Class, _VersionName, Rest} ->
	    _ = lager:debug("[Sender] [Associate] Association accepted"),
	    _ = lager:debug("[Sender] [Associate] Context ~p", [Contexts]),
	    {next_state, send_data, SenderData#sender_data{data = Rest, maxpdu = MaxPDU}};
	{error, Data, Error}  ->
	    _ = lager:warning("[Sender] [Associate] Association failed ~p", [Error]),
	    {keep_state, SenderData#sender_data{data = Data}, []}
    end;
associate(A, B, Data) ->
    _ = lager:warning("[Sender] [Associate] Unknown message ~p ~p", [A, B]),
    {keep_state, Data, []}.

%%-------------------------------------------------------------------
%% @doc Send Data  state.
%%
%% Split up the data according to the MaxPDU.
%% Send the chunks using a timeout.
%%
%% @end
%%-------------------------------------------------------------------

send_data(enter, _Prev, SenderData) ->
    ok = send_command_message(SenderData),
    #sender_data{flow = Flow, dataset = DataSet, maxpdu = MaxPDU} = SenderData,
    Encoded = wolfpacs_data_elements:encode(Flow, {explicit, little}, DataSet),
    Overhead = 6,
    Chunks = wolfpacs_utils:chunk(Encoded, MaxPDU - Overhead),
    {keep_state, SenderData#sender_data{data = <<>>, chunks=Chunks}, [{timeout, 0, send_more}]};

send_data(timeout, send_more, SenderData=#sender_data{chunks=[]}) ->
    _ = lager:debug("[Sender] [SendData] No more chunks to send"),
    {next_state, release, SenderData};

send_data(timeout, send_more, SenderData=#sender_data{chunks=[Chunk]}) ->
    _ = lager:debug("[Sender] [SendData] Send last chunk"),
    #sender_data{flow = Flow, sock = Sock} = SenderData,
    PDVItem = #pdv_item{pr_cid=1,
			is_last=true,
			is_command=false,
			pdv_data=Chunk},
    PDataTF = wolfpacs_p_data_tf:encode(Flow, [PDVItem]),
    ok = gen_tcp:send(Sock, PDataTF),
    {next_state, release, SenderData#sender_data{chunks=[]}};

send_data(timeout, send_more, SenderData) ->
    _ = lager:debug("[Sender] [SendData] Send anouther chunk"),
    #sender_data{flow = Flow, from = From, chunks = [Chunk|Chunks], sock = Sock} = SenderData,
    PDVItem = #pdv_item{pr_cid=1,
			is_last=false,
			is_command=false,
			pdv_data=Chunk},
    PDataTF = wolfpacs_p_data_tf:encode(Flow, [PDVItem]),
    case gen_tcp:send(Sock, PDataTF) of
	ok ->
	    {keep_state, SenderData#sender_data{chunks=Chunks}, [{timeout, 0, send_more}]};
	Error ->
	    %% TODO - We need to figure out what to do here
	    wolfpacs_flow:failed(Flow, ?MODULE, "unable to send"),
	    {keep_state, SenderData, [{reply, From, Error}]}
    end;

send_data(info, {tcp, _, <<7, _/binary>>}, SenderData) ->
    {next_state, release, SenderData};

send_data(A, B, SenderData) ->
    _ = lager:warning("[Sender] [SendData] Unknown message ~p ~p", [A, B]),
    {keep_state, SenderData, []}.

%%-------------------------------------------------------------------
%% @doc Release state.
%%
%% Send release
%%
%% @end
%%-------------------------------------------------------------------

release(enter, _Prev, SenderData) ->
    #sender_data{sock = Sock} = SenderData,
    ReleaseRQ = wolfpacs_release_rq:encode(),
    _ = lager:debug("[Sender] [Release] Send release request"),
    ok = gen_tcp:send(Sock, ReleaseRQ),
    {keep_state, SenderData, []};

release(info, {tcp, _Port, DataNew}, SenderData) ->
    #sender_data{flow = Flow, data = DataOld} = SenderData,
    Data = <<DataOld/binary, DataNew/binary>>,
    case wolfpacs_p_data_tf:decode(Flow, Data) of
	{ok, [#pdv_item{pdv_data=Payload}], Rest} ->
	    _ = wolfpacs_data_elements:decode(Flow, {explicit, little}, Payload),
	    _ = lager:debug("[Sender] [Release] Release complete"),
	    {next_state, finish, SenderData#sender_data{data = Rest}};
	_ ->
	    {keep_state, SenderData#sender_data{data = Data}, []}
    end;

release(A, B, SenderData) ->
    _ = lager:warning("[Sender] [Release] Unknown message ~p ~p", [A, B]),
    {keep_state, SenderData, []}.

%%-------------------------------------------------------------------
%% @doc Finish state.
%%
%% Send release
%%
%% @end
%%-------------------------------------------------------------------

finish(enter, _Prev, SenderData) ->
    _ = lager:debug("[Sender] [Finish] Waiting for release response"),
    {keep_state, SenderData, [{timeout, 2000, close}]};

finish(timeout, close, SenderData) ->
    #sender_data{sock = Sock, from = From} = SenderData,
    gen_tcp:close(Sock),
    _ = lager:debug("[Sender] [Finish] Closed socket due to timeout"),
    {keep_state, SenderData, [{reply, From, ok}]};

finish(info, {tcp, _, <<6, _/binary>>}, SenderData) ->
    #sender_data{sock = Sock, from = From} = SenderData,
    gen_tcp:close(Sock),
    _ = lager:debug("[Sender] [Finish] Release response. Closed socket"),
    {keep_state, SenderData, [{reply, From, ok}]};

finish(info, {tcp_closed, _Port}, SenderData) ->
    _ = lager:debug("[Sender] [Finish] Received TCP closed"),
    {keep_state, SenderData, []};

finish(A, B, SenderData) ->
    _ = lager:warning("[Sender] [Finish] Unknown message ~p ~p", [A, B]),
    {keep_state, SenderData, []}.

%%==============================================================================
%% Private
%%==============================================================================

send_command_message(SenderData) ->
    #sender_data{flow = Flow,
		 sock = Sock,
		 abstract_syntax = AbstractSyntax,
		 instance_uid = InstanceUID
		} = SenderData,
    Encoded = wolfpacs_c_store_rq:encode(Flow, {explicit, little}, AbstractSyntax, InstanceUID),
    PDVItem = #pdv_item{pr_cid=1,
			is_last=true,
			is_command=true,
			pdv_data=Encoded},
    PDataTF = wolfpacs_p_data_tf:encode(Flow, [PDVItem]),
    gen_tcp:send(Sock, PDataTF).

int(Value) when is_list(Value) ->
    list_to_integer(Value);
int(Value) when is_integer(Value) ->
    Value.
