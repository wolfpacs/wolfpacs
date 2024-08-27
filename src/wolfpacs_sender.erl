%%%-------------------------------------------------------------------
%% @author Niklas Johansson <raphexion@gmail.com>
%%
%% @copyright 2019, Niklas Johansson <raphexion@gmail.com>
%%
%% WolfPACS is a DICOM load-balancer.
%% Copyright (C) 2019  Niklas Johansson
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
%%
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

-export([start_link/1,
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

start_link(Remote) ->
    gen_statem:start_link(?MODULE, Remote, []).

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
		      strategy :: {atom(), atom()},
		      instance_uid :: binary(),
		      data :: binary(),
		      maxpdu :: non_neg_integer(),
		      chunks :: list(binary())}).

%% @hidden
init(Remote) ->
    #wolfpacs_remote{ host = Host
		    , port = Port
		    , ae = AE } = Remote,

    {ok, Flow} = wolfpacs_flow:start_link(),
    SenderData = #sender_data{flow = Flow,
			      host = Host,
			      port = Port,
			      called = AE,
			      calling = <<"WolfPACS">>,
			      sock = none,
			      from = none,
			      dataset = #{},
			      abstract_syntax = <<>>,
			      strategy = {implicit, little},
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
    logger:debug("[Sender] [Idel] Enter"),
    {keep_state, SenderData, []};
idle({call, From}, {send, DataSet, AbstractSyntax, InstanceUID}, SenderData) ->
    logger:debug("[Sender] [Idle] Connect over tcp/ip"),
    #sender_data{flow = Flow, host = Host, port = Port} = SenderData,
    wolfpacs_flow:reset(Flow),
    logger:debug("[Sender] Conenct to ~p ~p", [Host, Port]),
    case gen_tcp:connect(s(Host), int(Port), [binary, {active, true}]) of
	{ok, Sock} ->
	    NewSenderData = SenderData#sender_data{from = From,
						   sock = Sock,
						   dataset = DataSet,
						   abstract_syntax = AbstractSyntax,
						   instance_uid = InstanceUID
						  },
	    {next_state, associate, NewSenderData};
	{error, Error} ->
	    logger:warning("[Sender] [Idle] Unable to connect to ~p: ~p", [Host, Error]),
	    {keep_state, SenderData, [{reply, From, {error, Error}}]}
    end;
idle(A, B, SenderData) ->
    logger:warning("[Sender] [Idel] Unknown message ~p ~p", [A, B]),
    {keep_state, SenderData, []}.

%%-------------------------------------------------------------------
%% @doc Associate state.
%%
%% Will send the Associate Requests and wait for reply.
%%
%% @end
%%-------------------------------------------------------------------

associate(enter, _Prev, SenderData) ->
    logger:debug("[Sender] [Associate] Send Associate RQ"),
    #sender_data{flow = Flow,
		 sock = Sock,
		 called = CalledAE,
		 calling = CallingAE,
		 abstract_syntax = AbstractSyntax} = SenderData,

    Contexts = [{1, AbstractSyntax, [?EXPLICIT_LITTLE_ENDIAN,
				     ?IMPLICIT_LITTLE_ENDIAN,
				     ?EXPLICIT_BIG_ENDIAN]}],
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
	    logger:debug("[Sender] [Associate] Association accepted"),
	    logger:debug("[Sender] [Associate] Context ~p", [Contexts]),
	    Strategy = pick_strategy(Contexts),
	    {next_state, send_data, SenderData#sender_data{data = Rest,
							   maxpdu = MaxPDU,
							   strategy = Strategy}};
	{error, Data, Error}  ->
	    logger:warning("[Sender] [Associate] Association failed ~p", [Error]),
	    {keep_state, SenderData#sender_data{data = Data}, []}
    end;
associate(info, {tcp_closed, _Port}, SenderData) ->
    #sender_data{flow = Flow, from = From, abstract_syntax = AbstractSyntax} = SenderData,
    wolfpacs_flow:failed(Flow, ?MODULE, "Server closed socket"),
    logger:warning("[Sender] [Associate] Error: Server closed socket (~p)", [AbstractSyntax]),
    {keep_state, SenderData, [{reply, From, {error, failed_to_associate}}]};

associate(A, B, Data) ->
    logger:warning("[Sender] [Associate] Unknown message ~p ~p", [A, B]),
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
    #sender_data{flow = Flow,
		 dataset = DataSet,
		 maxpdu = MaxPDU,
		 strategy = Strategy} = SenderData,
    Encoded = wolfpacs_data_elements:encode(Flow, Strategy, DataSet),
    Overhead = 6,
    Chunks = wolfpacs_utils:chunk(Encoded, MaxPDU - Overhead),
    {keep_state, SenderData#sender_data{data = <<>>, chunks=Chunks}, [{timeout, 0, send_more}]};

send_data(timeout, send_more, SenderData=#sender_data{chunks=[]}) ->
    logger:debug("[Sender] [SendData] No more chunks to send"),
    {next_state, release, SenderData};

send_data(timeout, send_more, SenderData=#sender_data{chunks=[Chunk]}) ->
    logger:debug("[Sender] [SendData] Send last chunk"),
    #sender_data{flow = Flow, sock = Sock} = SenderData,
    PDVItem = #pdv_item{pr_cid=1,
			is_last=true,
			is_command=false,
			pdv_data=Chunk},
    PDataTF = wolfpacs_p_data_tf:encode(Flow, [PDVItem]),
    ok = gen_tcp:send(Sock, PDataTF),
    {next_state, release, SenderData#sender_data{chunks=[]}};

send_data(timeout, send_more, SenderData) ->
    logger:debug("[Sender] [SendData] Send anouther chunk"),
    #sender_data{flow = Flow, from = From, chunks = [Chunk|Chunks], sock = Sock} = SenderData,
    PDVItem = #pdv_item{pr_cid=1,
			is_last=false,
			is_command=false,
			pdv_data=Chunk},
    PDataTF = wolfpacs_p_data_tf:encode(Flow, [PDVItem]),
    case gen_tcp:send(Sock, PDataTF) of
	ok ->
	    {keep_state, SenderData#sender_data{chunks=Chunks}, [{timeout, 0, send_more}]};
	{error, closed} ->
	    wolfpacs_flow:failed(Flow, ?MODULE, "unable to send - closed"),
	    {keep_state, SenderData, [{reply, From, {error, closed}}]};
	Error ->
	    %% TODO - We need to figure out what to do here
	    wolfpacs_flow:failed(Flow, ?MODULE, "unable to send - retry"),
	    wolfpacs_flow:failed(Flow, ?MODULE, Error),
	    {keep_state, SenderData, [{timeout, 0, send_more}]}
    end;

send_data(info, {tcp, _, <<7, _/binary>>}, SenderData) ->
    {next_state, release, SenderData};

send_data(A, B, SenderData) ->
    logger:warning("[Sender] [SendData] Unknown message ~p ~p", [A, B]),
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
    logger:debug("[Sender] [Release] Send release request"),
    ok = gen_tcp:send(Sock, ReleaseRQ),
    {keep_state, SenderData, []};

release(info, {tcp, _Port, DataNew}, SenderData) ->
    #sender_data{flow = Flow, data = DataOld} = SenderData,
    Data = <<DataOld/binary, DataNew/binary>>,
    handle_release_data(Flow, SenderData, Data);

release(A, B, SenderData) ->
    logger:warning("[Sender] [Release] Unknown message ~p ~p", [A, B]),
    {keep_state, SenderData, []}.

handle_release_data(Flow, SenderData, Data = <<16#4, _/binary>>) ->
    case wolfpacs_p_data_tf:decode(Flow, Data) of
	{ok, [#pdv_item{pdv_data=Payload}], Rest} ->
	    _ = wolfpacs_data_elements:decode(Flow, {explicit, little}, Payload),
	    logger:debug("[Sender] [Release] Release complete"),
	    {next_state, finish, SenderData#sender_data{data = Rest}};
	_ ->
	    {keep_state, SenderData#sender_data{data = Data}, []}
    end;
handle_release_data(_Flow, SenderData, Data = <<16#7, _/binary>>) ->
    logger:debug("[Sender] [Release] Abort received"),
    {keep_state, SenderData#sender_data{data = Data}, []}.

%%-------------------------------------------------------------------
%% @doc Finish state.
%%
%% Send release
%%
%% @end
%%-------------------------------------------------------------------

finish(enter, _Prev, SenderData) ->
    logger:debug("[Sender] [Finish] Waiting for release response"),
    {keep_state, SenderData, [{timeout, 2000, close}]};

finish(timeout, close, SenderData) ->
    #sender_data{sock = Sock, from = From} = SenderData,
    gen_tcp:shutdown(Sock, read_write),
    logger:debug("[Sender] [Finish] Closed socket due to timeout"),
    {keep_state, SenderData, [{reply, From, ok}]};

finish(info, {tcp, _, <<6, _/binary>>}, SenderData) ->
    #sender_data{sock = Sock, from = From} = SenderData,
    gen_tcp:shutdown(Sock, read_write),
    logger:debug("[Sender] [Finish] Release response. Closed socket"),
    {keep_state, SenderData, [{reply, From, ok}]};

finish(info, {tcp_closed, _Port}, SenderData) ->
    logger:debug("[Sender] [Finish] Received TCP closed"),
    {keep_state, SenderData, []};

finish(A, B, SenderData) ->
    logger:warning("[Sender] [Finish] Unknown message ~p ~p", [A, B]),
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

s(String) when is_list(String) ->
    String;
s(Data) when is_binary(Data) ->
    binary_to_list(Data).

pick_strategy([{1, ?EXPLICIT_LITTLE_ENDIAN}]) ->
    {explicit, little};
pick_strategy([{1, ?IMPLICIT_LITTLE_ENDIAN}]) ->
    {implicit, little};
pick_strategy([{1, ?EXPLICIT_BIG_ENDIAN}]) ->
    {explicit, big};
pick_strategy(Contexts) ->
    logger:warning("[Sender] Unable to pick transfer syntax (~p)", [Contexts]),
    %% Pick the default, universially supported implicit little
    {implicit, little}.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

missing_class_uid_test() ->
    DataSet = #{},
    {ok, Sender} = start_link(#wolfpacs_remote{}),
    Res = send(Sender, DataSet),
    ?assertEqual(Res, {error, sop_class_uid_missing}).

missing_instance_uid_test() ->
    DataSet = #{?SOP_CLASS_UID => <<"class uid">>},
    {ok, Sender} = start_link(#wolfpacs_remote{}),
    Res = send(Sender, DataSet),
    ?assertEqual(Res, {error, sop_instance_uid_missing}).
