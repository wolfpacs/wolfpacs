-module(dcmtk_storescu).
-behaviour(gen_server).

-export([start_link/0,
	 stop/1,
	 send/4,
	 send_dataset/4]).

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
    gen_server:call(StoreSCU, {send, Host, Port, Filename}, 15000).

send_dataset(StoreSCU, Host, Port, DataSet) ->
    {ok, Flow} = wolfpacs_flow:start_link(),
    Content = wolfpacs_data_elements:encode(Flow, {explicit, little}, DataSet),
    Encoded = wolfpacs_file_format:encode(Flow, {explicit, little}, Content),
    Filename = generate_filename(DataSet),
    logger:warning("SEND ~p", [Filename]),
    ok = file:write_file(Filename, Encoded),
    gen_server:call(StoreSCU, {send, Host, Port, Filename}),
    %% It is much more stable if we wait removing the filename
    spawn(fun() ->
		  timer:sleep(5000),
		  file:delete(Filename)
	  end),
    wolfpacs_flow:stop(Flow).

init(_) ->
    {ok, #{process => none, from => none}}.

handle_call(stop, _From, State=#{process := none}) ->
    {reply, {ok, closing}, State};

handle_call(stop, _From, State=#{process := Process}) ->
    port_close(Process),
    {reply, {ok, closing}, State};

handle_call({send, Host, Port, Filename}, From, State) ->
    logger:warning("** SENDING ** ~p ~p ~p", [Host, Port, Filename]),
    Cmd = {spawn_executable, os:find_executable("storescu")},
    Options = [exit_status, use_stdio, stderr_to_stdout,
	       {line, 4096}, {args, ["-aec", "ninja", "-v", "-d", Host, str(Port), Filename]}],
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
    logger:warning("[DCMTKStoreSCU] Unable to send"),
    gen_server:reply(From, {error, N}),
    {noreply, State#{process => none, from => none}};

handle_info({_Port, {data, {eol, _Msg}}}, State) ->
    {noreply, State};

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, #{from := none}) ->
    ok;
terminate(Reason, #{from := From}) ->
    gen_server:reply(From, {error, Reason}),
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

generate_filename(#{{16#0008, 16#0018} := SOPInstanceUID}) ->
    "/tmp/" ++ binary_to_list(SOPInstanceUID) ++ ".dcm";
generate_filename(_) ->
    {A, B, C} = erlang:timestamp(),
    lists:flatten(io_lib:format("~p-~p.~p.~p",[wolfpacs, A, B, C])).

str(Value) when is_integer(Value) ->
    integer_to_list(Value);
str(Value) when is_list(Value) ->
    Value.
