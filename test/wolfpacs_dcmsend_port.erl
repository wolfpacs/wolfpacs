-module(wolfpacs_dcmsend_port).
-behaviour(gen_server).

-export([start_link/2,
	 stop/1]).
-export([send/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Host, Port) ->
    gen_server:start_link(?MODULE, {Host, port(Port)}, []).

stop(StoreSCU) ->
    gen_server:stop(StoreSCU).

send(StoreSCU, Filename) ->
    gen_server:call(StoreSCU, {send, Filename}).

init({Host, Port}) ->
    {ok, #{host => Host, port => Port}}.

handle_call({send, Filename}, From, State=#{host := Host, port := Port}) ->
    Args = ["-aec", "ninja", Host, Port, Filename],
    Options = [eof, {line, 4096}, exit_status, {args, Args}],
    Proc = open_port({spawn_executable, dcmsend_path()}, Options),
    {noreply, State#{from => From, proc => Proc}}.

%% Resolve dcmsend path in a way that works both on CI and in Nix shells.
%% You can override via the DCMSEND env var.
dcmsend_path() ->
    case os:getenv("DCMSEND") of
        false ->
            case os:find_executable("dcmsend") of
                false -> "/usr/bin/dcmsend";
                Path -> Path
            end;
        Path -> Path
    end.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({_, {exit_status, N}}, State=#{from := From}) ->
    gen_server:reply(From, {ok, exit_status, N}),
    {noreply, State};

handle_info({_, eof}, #{host := Host, port := Port}) ->
    {noreply, #{host => Host, port => Port}};

handle_info(What, State) ->
    logger:warning("info ~p", [What]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

port(Port) when is_integer(Port) ->
    integer_to_list(Port);
port(Port) ->
    Port.
