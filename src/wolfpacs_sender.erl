%%%-------------------------------------------------------------------
%% @doc Sender
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_sender).
-behaviour(gen_server).
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").

-export([start_link/4,
	 stop/1,
	 send/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Host, Port, CalledAE, CallingAE) ->
    gen_server:start_link(?MODULE, [Host, Port, CalledAE, CallingAE], []).

stop(Sender) ->
    gen_server:stop(Sender).

send(Sender, DataSet) ->
    gen_server:call(Sender, {send, DataSet}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init([Host, Port, CalledAE, CallingAE]) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    State = #{flow => Flow,
	      host => Host,
	      port => Port,
	      called => CalledAE,
	      calling => CallingAE,
	      sock => none,
	      from => none,
	      dataset => #{},
	      data => <<>>},
    {ok, State}.

%% @hidden
handle_call({send, DataSet}, From, State=#{sock := none}) ->
    #{flow := Flow, host := Host, port := Port} = State,
    wolfpacs_flow:reset(Flow),
    case gen_tcp:connect(Host, Port, [binary, {active, true}]) of
	{ok, Sock} ->
	    AbstractSyntax = maps:get({16#0008, 16#0016}, DataSet, missing),
	    {noreply, send_associate_rq(AbstractSyntax,
					State#{from => From,
					       sock => Sock,
					       dataset => DataSet})};
	{error, Error} ->
	    {reply, {error, Error}, State}
    end;

%% @hidden
handle_call({send, _}, _From, State) ->
    {reply, {error, busy}, State};

%% @hidden
handle_call(What, _From, State) ->
    _ = lager:warning("[Sender] unhandle call ~p", [What]),
    {reply, {error, What}, State}.

%% @hidden
handle_cast(What, State) ->
    _ = lager:warning("[Sender] unhandle cast ~p", [What]),
    {noreply, State}.

%% @hidden
handle_info({tcp, _Port, DataNew}, State=#{data := DataOld}) ->
    Data = <<DataOld/binary, DataNew/binary>>,
    handle_data(Data, State);

%% @hidden
handle_info(What, State) ->
    _ = lager:warning("[Sender] unhandle info ~p", [What]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Private
%%==============================================================================

%%
%
%%

send_associate_rq(missing, State) ->
    State;
send_associate_rq(AbstractSyntax, State) ->
    #{sock := Sock,
      called := CalledAE,
      calling := CallingAE} = State,

    Contexts = [{1, AbstractSyntax, [?EXPLICIT_LITTLE_ENDIAN]}],
    MaxPDUSize = 16384,
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>,
    VersionName = <<"WolfPACS_000">>,

    AssociateRQ = wolfpacs_associate_rq:encode(CalledAE,
					       CallingAE,
					       Contexts,
					       MaxPDUSize,
					       Class,
					       VersionName),
    ok = gen_tcp:send(Sock, AssociateRQ),
    State.

%%
%
%%

handle_data(Data = <<16#2, _/binary>>, State) ->
    case wolfpacs_associate_ac:decode(Data) of
	{ok, _CalledAE, _CallingAE, _R, _Contexts, _, _Class, _VersionName, Rest} ->
	    {noreply, send_release_rq(State#{data => Rest})};
	{error, Data, Error}  ->
	    _ = lager:warning("[Sender] associate_ac decode error ~p", [Error]),
	    {stop, normal, State}
    end;
handle_data(Data = <<16#6, _/binary>>, State=#{from := From, sock := Sock}) ->
    case wolfpacs_release_rp:decode(Data) of
	{ok, _, Rest} ->
	    gen_server:reply(From, {ok, success}),
	    gen_tcp:close(Sock),
	    {noreply, State#{sock => none, from => none, data => Rest}};
	{error, Data} ->
	    _ = lager:warning("[Sender] release_rp decode error"),
	    {stop, normal, State}
    end;
handle_data(Data, State) ->
    _ = lager:warning("[Sender] unable to handle data ~p", [Data]),
    {noreply, State}.

%%
%
%%

send_release_rq(State=#{sock := Sock}) ->
    ReleaseRQ = wolfpacs_release_rq:encode(),
    ok = gen_tcp:send(Sock, ReleaseRQ),
    State.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").
