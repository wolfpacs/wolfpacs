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
    gen_server:cast(?MODULE, {remember, clean_ref(Ref), StudyUID}).

route(StudyUID, DataSet) ->
    gen_server:cast(?MODULE, {route, StudyUID, DataSet}).

set_destination(Ref, Host, Port) ->
    gen_server:cast(?MODULE, {set_destination, clean_ref(Ref), Host, Port}).

remove_destination(Ref) ->
    gen_server:cast(?MODULE, {remove_destination, clean_ref(Ref)}).

number_of_destinations() ->
    gen_server:call(?MODULE, number_of_destinations).

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
handle_call(number_of_destinations, _From, State) ->
    #{hosts := Hosts} = State,
    {reply, {ok, maps:size(Hosts)}, State};
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({route, StudyUID, DataSet}, State=#{hosts := Hosts, studies := Studies}) ->
    case maps:get(StudyUID, Studies, missing) of
	missing ->
	    _ = lager:warning("[InsideRouter] Study not mapped: ~p", [StudyUID]),
	    {noreply, State};
	{CalledAE, CallingAE} ->
	    Host = maps:get({CalledAE, CallingAE}, Hosts, missing),
	    priv_route(DataSet, CalledAE, CallingAE, Host),
	    {noreply, State}
    end;

handle_cast({remember, Ref, StudyUID}, State=#{studies := Studies}) ->
    _ = lager:debug("[InsideRouter] Remember: ~p", [Ref]),
    {noreply, State#{studies => Studies#{StudyUID => Ref}}};

handle_cast({set_destination, Ref, Host, Port}, State=#{hosts := Hosts}) ->
    _ = lager:debug("[InsideRouter] New destination: ~p", [Ref]),
    {noreply, State#{hosts => Hosts#{Ref => {Host, Port}}}};

handle_cast({remove_destination, Ref}, State=#{hosts := Hosts}) ->
    {noreply, State#{hosts => maps:remove(Ref, Hosts)}};

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
    _ = lager:warning("[InsideRouter] Host not mapped for ~p ~p", [CalledAE, CallingAE]),
    ok;
priv_route(DataSet, CalledAE, CallingAE, {Host, Port}) ->
    {ok, Sender} = wolfpacs_sender:start_link(Host, Port, CalledAE, CallingAE),
    wolfpacs_sender:send(Sender, DataSet),
    wolfpacs_sender:stop(Sender).

trim(Item) when is_binary(Item) ->
    trim(binary_to_list(Item));
trim(Item) ->
    string:strip(string:strip(Item, right, 0), right, 32).

clean_ref({A, B}) ->
    {trim(A), trim(B)};
clean_ref(Ref) ->
    Ref.

%%-----------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

start_debug_info_stop_test() ->
    {ok, _} = start_link(),
    {ok, _} = debug(),
    ?MODULE ! test_message,
    ok = stop().

minimal_destinations_test() ->
    {ok, _} = start_link(),
    {ok, 0} = number_of_destinations(),
    ok = set_destination({"foo", "bar"}, "localhost", 1234),
    {ok, 1} = number_of_destinations(),
    ok = set_destination({"foo", "bar"}, "localhost", 1234),
    {ok, 1} = number_of_destinations(),
    ok = set_destination({"fooy", "bary"}, "localhost", 1234),
    {ok, 2} = number_of_destinations(),
    ok = remove_destination({"foo", "bar"}),
    {ok, 1} = number_of_destinations(),
    ok = remove_destination({"foo", "bar"}),
    {ok, 1} = number_of_destinations(),
    ok = remove_destination({"fooy", "bary"}),
    {ok, 0} = number_of_destinations(),
    ok = stop().
