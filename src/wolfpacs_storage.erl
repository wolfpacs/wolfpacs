-module(wolfpacs_storage).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 empty/0,
	 store/1,
	 retreive/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

empty() ->
    gen_server:cast(?MODULE, empty).

store(Items) ->
    gen_server:cast(?MODULE, {store, Items}).

retreive() ->
    gen_server:call(?MODULE, retreive).

init(_) ->
    {ok, #{last => #{}}}.

handle_call(retreive, _From, State=#{last := Last}) ->
    {reply, {ok, Last}, State}.

handle_cast(empty, State) ->
    {noreply, State#{last => #{}}};

handle_cast({store, Items}, State) ->
    {noreply, State#{last => Items}};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
