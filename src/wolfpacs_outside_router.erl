-module(wolfpacs_outside_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("wolfpacs_types.hrl").

-export([start_link/0,
	 stop/0,
	 route/4,
	 add_worker/3,
	 remove_worker/3,
	 workers/0,
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
    gen_server:call(?MODULE, {add_worker, Host, Port, AE}).

remove_worker(Host, Port, AE) ->
    gen_server:cast(?MODULE, {remove_worker, Host, Port, AE}).

workers() ->
    gen_server:call(?MODULE, workers).

number_of_workers() ->
    gen_server:call(?MODULE, number_of_workers).

debug() ->
    gen_server:call(?MODULE, debug).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    State = #{workers => [], study_workers => #{}},
    {ok, State}.

handle_call(debug, _From, State) ->
    {reply, {ok, State}, State};

handle_call(number_of_workers, _From, State=#{workers := Workers}) ->
    {reply, {ok, length(Workers)}, State};

handle_call(workers, _From, State=#{workers := Workers}) ->
    {reply, {ok, Workers}, State};

handle_call({add_worker, Host, Port, AE}, _From, State=#{workers := Workers}) ->
    NewID = length(Workers),
    Worker = #wolfpacs_worker{id = NewID, host = Host, port = Port, ae = AE, state = unseen},
    {reply, {ok, NewID}, State#{workers => [Worker|Workers]}};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({route, _CalledAE, _CallingAE, DataSet, StudyUID}, State) ->
    #{workers := Workers, study_workers := StudyMap} = State,
    Known = maps:get(StudyUID, StudyMap, missing),
    case find_worker(Workers, Known) of
	missing ->
	    _ = lager:warning("[OutsideRouter] Study wasn't previously seen. No worker found"),
	    {noreply, State};
	SendInfo ->
	    priv_send(DataSet, SendInfo),
	    {noreply, update_studyuid_map(StudyUID, SendInfo, State)}
    end;

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

find_worker(Workers, missing) ->
    OnlineWorkers = lists:filter(fun online/1, Workers),
    random_worker(OnlineWorkers);
find_worker(_Workers, SendInfo) ->
    SendInfo.

online(#wolfpacs_worker{state = online}) ->
    true;
online(_) ->
    false.

random_worker(Workers) ->
    Index = rand:uniform(length(Workers)),
    lists:nth(Index, Workers).

priv_send(DataSet, {Host, Port, CalledAE}) ->
    CallingAE = <<"WolfPACS">>,
    wolfpacs_sender_pool:send(Host, Port, CalledAE, CallingAE, DataSet).

update_studyuid_map(StudyUID, #wolfpacs_worker{id = Id}, State=#{study_workers := Map}) ->
    State#{study_workers => Map#{StudyUID => Id}}.

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
    {ok, _} = add_worker("localhost", 1234, "AE1"),
    {ok, 1} = number_of_workers(),
    {ok, _} = add_worker("localhost", 1234, "AE2"),
    {ok, 2} = number_of_workers(),
    ok = stop().

start_stop_test() ->
    start_link(),
    ?assertEqual(stop(), ok).

cast_test() ->
    start_link(),
    gen_server:cast(?MODULE, this_should_not_crash),
    ?assertEqual(stop(), ok).

info_test() ->
    start_link(),
    ?MODULE ! this_should_not_crash,
    ?assertEqual(stop(), ok).

code_change_test() ->
    start_link(),
    ?assertEqual(code_change(1, state, extra), {ok, state}).
