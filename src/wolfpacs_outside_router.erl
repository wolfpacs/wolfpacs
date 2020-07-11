-module(wolfpacs_outside_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 stop/0,
	 route/3,
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

route(CalledAE, CallingAE, DataSet) ->
    gen_server:cast(?MODULE, {route, CalledAE, CallingAE, DataSet}).

add_worker(Host, Port, AE) ->
    gen_server:cast(?MODULE, {add_worker, Host, Port, AE}).

remove_worker(Host, Port, AE) ->
    gen_server:cast(?MODULE, {remove_worker, Host, Port, AE}).

debug() ->
    gen_server:call(?MODULE, debug).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    State = #{workers => #{},
	      next_worker => 0,
	      nb_workers => 0
	     },
    {ok, State}.

handle_call(debug, _From, State) ->
    {reply, {ok, State}, State};
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({route, _CalledAE, _CallingAE, DataSet},  State=#{nb_workers := 0}) ->
    lager:warning("[OutsideRouter] Drop dataset. No workers. Saving"),
    wolfpacs_storage:store(DataSet),
    {noreply, State};
handle_cast({route, _CalledAE, _CallingAE, DataSet},  State=#{workers := Workers, next_worker := I}) ->
    lager:warning("OUTSIDE ROUTE"),
    case priv_route(DataSet, maps:get(I, Workers)) of
	error ->
	    lager:warning("[OutsideRouter] Drop dataset. Unable to propagate"),
	    {noreply, State};
	_ ->
	    {noreply, round_robin(State)}
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

priv_route(DataSet, {Host, Port, _AE}) ->
    {ok, Sender} = dcmtk_storescu:start_link(),
    dcmtk_storescu:send_dataset(Sender, Host, Port, DataSet),
    dcmtk_storescu:stop(Sender).

round_robin(State=#{next_worker := I, nb_workers := N}) ->
    J = (I + 1) rem N,
    State#{next_worker => J}.
