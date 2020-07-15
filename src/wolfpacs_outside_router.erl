-module(wolfpacs_outside_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 stop/0,
	 route/4,
	 add_worker/3,
	 remove_worker/3,
	 debug/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

route(CalledAE, CallingAE, DataSet, StudyUID) ->
    gen_server:cast(?MODULE, {route, CalledAE, CallingAE, DataSet, StudyUID}).

add_worker(Host, Port, AE) ->
    gen_server:cast(?MODULE, {add_worker, Host, Port, AE}).

remove_worker(Host, Port, AE) ->
    gen_server:cast(?MODULE, {remove_worker, Host, Port, AE}).

number_of_workers() ->
    gen_server:call(?MODULE, number_of_workers).

debug() ->
    gen_server:call(?MODULE, debug).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    State = #{workers => #{},
	      next_worker => 0,
	      nb_workers => 0,
	      study_workers => #{}
	     },
    {ok, State}.

handle_call(debug, _From, State) ->
    {reply, {ok, State}, State};
handle_call(number_of_workers, _From, State=#{nb_workers := N}) ->
    {reply, {ok, N}, State};
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({route, _CalledAE, _CallingAE, DataSet},  State=#{nb_workers := 0}) ->
    _ = lager:warning("[OutsideRouter] Drop dataset. No workers. Saving"),
    wolfpacs_storage:store(DataSet),
    {noreply, State};
handle_cast({route, _CalledAE, _CallingAE, DataSet, StudyUID}, State) ->
    #{workers := Workers, next_worker := I, study_workers := StudyMap} = State,
    case maps:get(StudyUID, StudyMap, missing) of
	missing ->
	    case maps:get(I, Workers, missing) of
		missing ->
		    _ = lager:warning("[OutsideRouter] Study wasn't previously seen. No worker found"),
		    wolfpacs_storage:store(DataSet),
		    {noreply, State};
		SendInfo ->
		    priv_send(DataSet, SendInfo),
		    {noreply, round_robin_and_note_studyuid(State, StudyUID, SendInfo)}
	    end;
	SendInfo ->
	    priv_send(DataSet, SendInfo),
	    {noreply, State}
    end;

handle_cast({add_worker, Host, Port, AE}, State=#{workers := Workers, nb_workers := I}) ->
    {noreply, State#{workers => Workers#{I => {Host, Port, AE}}, nb_workers => (I + 1)}};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

priv_send(DataSet, {Host, Port, CalledAE}) ->
    CallingAE = <<"WolfPACS">>,
    {ok, Sender} = wolfpacs_sender:start_link(Host, Port, CalledAE, CallingAE),
    wolfpacs_sender:send(Sender, DataSet),
    wolfpacs_sender:stop(Sender).

round_robin_and_note_studyuid(State, StudyUID, SendInfo) ->
    #{next_worker := I, nb_workers := N, study_workers := StudyMap} = State,
    J = (I + 1) rem N,
    State#{next_worker => J, study_workers => StudyMap#{StudyUID => SendInfo}}.

%%-----------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

start_debug_info_stop_test() ->
    {ok, _} = start_link(),
    {ok, _} = debug(),
    ?MODULE ! test_message,
    ok = stop().

minimal_worker_test() ->
    {ok, _} = start_link(),
    {ok, 0} = number_of_workers(),
    ok = add_worker("localhost", 1234, "AE1"),
    {ok, 1} = number_of_workers(),
    ok = add_worker("localhost", 1234, "AE2"),
    {ok, 2} = number_of_workers(),
    ok = stop().
