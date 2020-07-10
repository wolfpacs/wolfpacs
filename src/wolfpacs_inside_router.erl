-module(wolfpacs_inside_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 stop/0,
	 remember/2,
	 route/2,
	 set_destination/3,
	 remove_destination/1,
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

remember(missing, _) ->
    ok;
remember(Ref, StudyUID) ->
    gen_server:cast(?MODULE, {remember, Ref, StudyUID}).

route(StudyUID, DataSet) ->
    gen_server:cast(?MODULE, {route, StudyUID, DataSet}).

set_destination(Ref, Host, Port) ->
    gen_server:cast(?MODULE, {set_destination, Ref, Host, Port}).

remove_destination(Ref) ->
    gen_server:cast(?MODULE, {remove_destination, Ref}).

debug() ->
    gen_server:call(?MODULE, debug).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    State = #{hosts => #{},
	      studies => #{}},
    {ok, State}.

handle_call(debug, _From, State) ->
    {reply, {ok, State}, State};
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({route, StudyUID, DataSet}, State=#{hosts := Hosts, studies := Studies}) ->
    lager:warning("OUTSIDE INSIDE"),
    case maps:get(StudyUID, Studies, missing) of
	missing ->
	    lager:warning("[InsideRouter] Study not mapped: ~p", [StudyUID]),
	    {noreply, State};
	{CalledAE, CallingAE} ->
	    Host = maps:get({CalledAE, CallingAE}, Hosts, missing),
	    priv_route(DataSet, CalledAE, CallingAE, Host),
	    {noreply, State}
    end;

handle_cast({remember, Ref, StudyUID}, State=#{studies := Studies}) ->
    lager:warning("REMEMBER: ~p", [Ref]),
    {noreply, State#{studies => Studies#{StudyUID => Ref}}};

handle_cast({set_destination, Ref, Host, Port}, State=#{hosts := Hosts}) ->
    lager:warning("DESTINATION: ~p", [Ref]),
    {noreply, State#{hosts => Hosts#{Ref => {Host, Port}}}};

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

priv_route(_DataSet, CalledAE, CallingAE, missing) ->
    lager:warning("[InsideRouter] Host not mapped for ~p ~p", [CalledAE, CallingAE]),
    ok;
priv_route(DataSet, _CalledAE, _CallingAE, {Host, Port}) ->
    {ok, Sender} = dcmtk_storescu:start_link(),
    dcmtk_storescu:send_dataset(Sender, Host, Port, DataSet),
    dcmtk_storescu:stop(Sender).

%%-----------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
