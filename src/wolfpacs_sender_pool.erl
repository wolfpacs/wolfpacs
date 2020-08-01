-module(wolfpacs_sender_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(PARALLEL_SENDS, 25).

-export([start_link/0,
	 stop/0,
	 send/5,
	 done/0,
	 size/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-record(sender_info, {host :: string(),
		      port :: integer(),
		      called :: binary(),
		      calling :: binary(),
		      dataset :: map(),
		      retries :: integer()
		     }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

send(Host, Port, CalledAE, CallingAE, DataSet) ->
    SenderInfo = #sender_info{host = s(Host),
			      port = i(Port),
			      called = b(CalledAE),
			      calling = b(CallingAE),
			      dataset = DataSet,
			      retries = 0},
    gen_server:cast(?MODULE, {send, SenderInfo}).

done() ->
    gen_server:cast(?MODULE, done).

retry(SenderInfo) ->
    #sender_info{retries = Retries} = SenderInfo,
    gen_server:cast(?MODULE, {retry, SenderInfo#sender_info{retries = Retries + 1}}).

size() ->
    gen_server:call(?MODULE, size).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    Queue = queue:new(),
    {ok, Queue}.

handle_call(size, _From, Queue) ->
    {reply, {ok, queue:len(Queue)}, Queue};

handle_call(What, _From, Queue) ->
    {reply, {error, What}, Queue}.

handle_cast({send, SenderInfo}, Queue) ->
    {noreply, queue:in(SenderInfo, Queue), 0};

handle_cast({retry, SenderInfo}, Queue) ->
    {noreply, queue:in(SenderInfo, Queue), 0};

handle_cast(done, Queue) ->
    Len = queue:len(Queue),
    handle_timeout(Queue, Len == 0, Len < ?PARALLEL_SENDS);

handle_cast(_What, Queue) ->
    {noreply, Queue}.

handle_info(timeout, Queue) ->
    Len = queue:len(Queue),
    handle_timeout(Queue, Len == 0, Len < ?PARALLEL_SENDS);

handle_info(_What, Queue) ->
    {noreply, Queue}.

terminate(_Reason, _Queue) ->
    ok.

code_change(_Vsn, Queue, _Extra) ->
    {ok, Queue}.

%%==============================================================================
%% Private
%%==============================================================================

handle_timeout(Queue, true, _) ->
    {noreply, Queue, hibernate};
handle_timeout(QueueIn, false, true) ->
    {{value, SenderInfo}, QueueOut} = queue:out(QueueIn),
    spawn(fun() ->
		  #sender_info{host = Host,
			       port = Port,
			       called = CalledAE,
			       calling = CallingAE,
			       dataset = DataSet,
			       retries = Retries} = SenderInfo,
		  lager:debug("[SenderPool] Retry: ~p", [Retries]),
		  {ok, Sender} = wolfpacs_sender:start_link(Host, Port, CalledAE, CallingAE),
		  case wolfpacs_sender:send(Sender, DataSet) of
		      ok ->
			  lager:debug("[SenderPool] Successfully send dataset"),
			  wolfpacs_sender:stop(Sender),
			  done();
		      Error ->
			  lager:warning("[SenderPool] Error: ~p", [Error]),
			  retry(SenderInfo)
		  end
	  end),
    {noreply, QueueOut, 0};
handle_timeout(Queue, _, _) ->
    {noreply, Queue, 100}.

i(Value) when is_list(Value) ->
    list_to_integer(Value);
i(Value) when is_integer(Value) ->
    Value.

b(Value) when is_list(Value) ->
    list_to_binary(Value);
b(Value) when is_binary(Value) ->
    Value.

s(Value) when is_binary(Value) ->
    binary_to_list(Value);
s(Value) when is_list(Value) ->
    Value.
