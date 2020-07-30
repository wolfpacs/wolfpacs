-module(wolfpacs_worker).
-behaviour(gen_server).

-export([start_link/3,
	 stop/1,
	 pause/1,
	 resume/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Host, Port, AE) ->
    gen_server:start_link(?MODULE, [Host, Port, AE], []).

stop(Worker) ->
    gen_server:stop(Worker).

pause(Worker) ->
    gen_server:cast(Worker, pause).

resume(Worker) ->
    gen_server:cast(Worker, resume).

init([Host, Port, AE]) ->
    {ok, #{host => Host, port => Port, ae => AE, paused => false}}.

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(pause, State) ->
    {noreply, State#{pause => true}};

handle_cast(resume, State) ->
    {noreply, State#{pause => false}};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
