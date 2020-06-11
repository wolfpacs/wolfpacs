-module(dcmtk_storescu).
-behaviour(gen_server).

-export([start_link/0,
	 stop/1,
	 send/4]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(StoreSCP) ->
    gen_server:call(StoreSCP, stop).

send(StoreSCU, Host, Port, Filename) ->
    gen_server:call(StoreSCU, {send, Host, Port, Filename}).

init(_) ->
    {ok, #{}}.

handle_call(stop, _From, State=#{process := none}) ->
    {reply, {ok, closing}, State};

handle_call(stop, _From, State=#{process := Process}) ->
    port_close(Process),
    {reply, {ok, closing}, State};

handle_call({send, Host, Port, Filename}, From, State) ->
    Cmd = {spawn_executable, os:find_executable("storescu")},
    Options = [exit_status, use_stdio, stderr_to_stdout,
	       {line, 255}, {args, ["-v", "-d", Host, Port, Filename]}],
    Process = open_port(Cmd, Options),
    {noreply, State#{process => Process, from => From}};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({_Port, {exit_status, 0}}, State=#{from := From}) ->
    gen_server:reply(From, ok),
    {noreply, State#{process => none, from => none}};

handle_info({_Port, {exit_status, N}}, State=#{from := From}) ->
    gen_server:reply(From, {error, N}),
    {noreply, State#{process => none, from => none}};

handle_info({_Port, {data, {eol, _Msg}}}, State) ->
    {noreply, State};

handle_info(What, State) ->
    lager:warning("Info ~p", [What]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:warning("Terminated with reason ~p", [Reason]),
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
