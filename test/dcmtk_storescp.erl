-module(dcmtk_storescp).
-behaviour(gen_server).

-export([start_link/1,
	 stop/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Port) when is_integer(Port) ->
    start_link(integer_to_list(Port));
start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).

stop(StoreSCP) ->
    gen_server:call(StoreSCP, stop).

init(Port) ->
    {ok, #{port => Port}, 0}.

handle_call(stop, _From, State=#{process := Process}) ->
    port_close(Process),
    {reply, {ok, closing}, State};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(timeout, State=#{port := Port}) ->
    process_flag(trap_exit, true),
    Cmd = {spawn_executable, os:find_executable("storescp")},
    Options = [exit_status, use_stdio, stderr_to_stdout, {line, 255}, {args, ["-v", "-d", Port]}],
    Process = open_port(Cmd, Options),
    {noreply, State#{process => Process}};

handle_info({_Port, {exit_status, 0}}, State) ->
    {stop, normal, State};
handle_info({_Port, {exit_status, N}}, State) ->
    {stop, N, State};

handle_info({_Port, {data, {eol, Msg}}}, State) ->
    lager:warning("~p", [Msg]),
    {noreply, State};

handle_info(What, State) ->
    lager:warning("Info ~p", [What]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:warning("Terminated with reason ~p", [Reason]),
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
