%%%-------------------------------------------------------------------
%% @doc C Echo SCU.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_c_echo_scu).
-behaviour(gen_server).
-include("abstract_syntax.hrl").
-include("transfer_syntax.hrl").

-export([start_link/0,
	 stop/1,
	 echo/5]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(CEchoSCU) ->
    gen_server:stop(CEchoSCU).

echo(CEchoSCU, Host, Port, CalledAE, Strategy) ->
    gen_server:call(CEchoSCU, {echo, Host, Port, CalledAE, Strategy}).

%% @hidden
init(_) ->
    {ok, #{sock => none,
	   from => none, data => <<>>,
	   strategy => {none, none}}}.

%% @hidden
handle_call({echo, Host, Port, CalledAE, Strategy}, From, State=#{sock := none}) ->
    case gen_tcp:connect(Host, Port, [binary, {active, true}]) of
	{ok, Sock} ->
	    {noreply, send_associate_rq(State#{calledae => CalledAE,
					       from => From, sock => Sock,
					       strategy => Strategy})};
	{error, Error} ->
	    {reply, {error, Error}, State}
    end;

%% @hidden
handle_call({echo, _, _, _}, _From, State) ->
    {reply, {error, busy}, State};

%% @hidden
handle_call(What, _From, State) ->
    ok = lager:warning("[c_echo_scu] unhandle call ~p", [What]),
    {reply, {error, What}, State}.

%% @hidden
handle_cast(What, State) ->
    ok = lager:warning("[c_echo_scu] unhandle cast ~p", [What]),
    {noreply, State}.

%% @hidden
handle_info({tcp, _Port, DataNew}, State=#{data := DataOld}) ->
    Data = <<DataOld/binary, DataNew/binary>>,
    handle_data(Data, State);

%% @hidden
handle_info(What, State) ->
    ok = lager:warning("[c_echo_scu] unhandle info ~p", [What]),
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

handle_data(Data = <<16#2, _/binary>>, State) ->
    case wolfpacs_associate_ac:decode(Data) of
	{ok, _CalledAE, _CallingAE, _R, _Contexts, _, _Class, _VersionName, Rest} ->
	    {noreply, send_release_rq(State#{data => Rest})};
	{error, Data, Error}  ->
	    ok = lager:warning("[c_echo_scu] associate_ac decode error ~p", [Error]),
	    {stop, normal, State}
    end;
handle_data(Data = <<16#6, _/binary>>, State=#{from := From, sock := Sock}) ->
    case wolfpacs_release_rp:decode(Data) of
	{ok, _, Rest} ->
	    gen_server:reply(From, {ok, success}),
	    gen_tcp:close(Sock),
	    {noreply, State#{sock => none, from => none, data => Rest}};
	{error, Data} ->
	    ok = lager:warning("[c_echo_scu] release_rp decode error"),
	    {stop, normal, State}
    end;
handle_data(Data, State) ->
    ok = lager:warning("[c_echo_scu] unable to handle data ~p", [Data]),
    {noreply, State}.

transfer_syntax({implicit, little}) ->
    ?IMPLICIT_LITTLE_ENDIAN;
transfer_syntax({explicit, little}) ->
    ?EXPLICIT_LITTLE_ENDIAN;
transfer_syntax({explicit, big}) ->
    ?EXPLICIT_BIG_ENDIAN;
transfer_syntax(TransferSyntax) ->
    ok = lager:warning("[c_echo_scu] Unknown transfer syntax ~p", [TransferSyntax]),
    ?EXPLICIT_LITTLE_ENDIAN.

send_associate_rq(State=#{sock := Sock, calledae := CalledAE_, strategy := Strategy}) ->
    PrCID = 1,
    AbstractSyntax = ?VERIFICATION,
    TransferSyntax = [transfer_syntax(Strategy)],
    MaxPDUSize = 16384,
    %% TODO, little or big here ?
    CallingAE = wolfpacs_vr_ae:encode(Strategy, <<"WolfPACS">>),
    CalledAE = wolfpacs_vr_ae:encode(Strategy, CalledAE_),
    Class = <<"1.2.276.0.7230010.3.0.3.6.4">>, %% TODO Change
    VersionName = <<"WolfPACS_000">>,

    AssociateRQ = wolfpacs_associate_rq:encode(CalledAE, CallingAE,
					       PrCID, AbstractSyntax, TransferSyntax,
					       MaxPDUSize, Class, VersionName),

    ok = gen_tcp:send(Sock, AssociateRQ),
    State.

send_release_rq(State=#{sock := Sock}) ->
    ReleaseRQ = wolfpacs_release_rq:encode(),
    ok = gen_tcp:send(Sock, ReleaseRQ),
    State.

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

bad_echo_test_() ->
    {ok, Echo} = start_link(),
    [ ?_assertEqual(echo(Echo, "localhost1234", 11112, "foo", {explicit, little}), {error, nxdomain})
    , ?_assertEqual(echo(Echo, "localhost", 21, "foo", {explicit, little}), {error, econnrefused})
    , ?_assertEqual(echo(Echo, "localhost", 21, "foo", {foo, bar}), {error, econnrefused})
    ].

gen_server_test_() ->
    {ok, Echo} = start_link(),
    Echo ! some_info, %% should not crash the server
    [ ?_assertEqual(gen_server:cast(Echo, foobar), ok),
      ?_assertEqual(gen_server:call(Echo, foobar), {error, foobar})
    ].
