%%%-------------------------------------------------------------------
%% @doc Authentication.
%%
%% @end
%%%-------------------------------------------------------------------

-module(wolfpacs_authentication).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 stop/0,
	 add/2,
	 add_calling/1,
	 add_called/1,
	 authenticate/2]).

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

add(CallingAE, CalledAE) ->
    gen_server:cast(?MODULE, {add, CallingAE, CalledAE}).

add_calling(CallingAE) ->
    gen_server:cast(?MODULE, {add_calling, CallingAE}).

add_called(CalledAE) ->
    gen_server:cast(?MODULE, {add_called, CalledAE}).

authenticate(CallingAE, CalledAE) ->
    gen_server:call(?MODULE, {authenticate, CallingAE, CalledAE}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    {ok, init_state(binary_env_var("CALLING_AE", missing),
		    binary_env_var("CALLED_AE", missing))}.

handle_call({authenticate, _CallingAE, _CalledAE}, _From, State=#{allow_all := true}) ->
    {reply, {ok, true}, State};
handle_call({authenticate, CallingAE, CalledAE}, _From, State) ->
    {reply, {ok, allowed(CallingAE, CalledAE, State)}, State}.

handle_cast({add, CallingAE, CalledAE},  State) ->
    {noreply, priv_add(CallingAE, CalledAE, State)};
handle_cast({add_calling, AE},  State) ->
    {noreply, priv_add_calling(AE, State)};
handle_cast({add_called, AE},  State) ->
    {noreply, priv_add_called(AE, State)};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

default() ->
    #{allow_all => true,
      calling => sets:new(),
      called => sets:new(),
      both => sets:new()}.

priv_add(CallingAE_, CalledAE_, State=#{both := Set}) ->
    CallingAE = reshape(CallingAE_),
    CalledAE = reshape(CalledAE_),
    State#{allow_all => false, both => sets:add_element({CallingAE, CalledAE}, Set)}.
priv_add_calling(AE_, State=#{calling := Set}) ->
    AE = reshape(AE_),
    State#{allow_all => false, calling => sets:add_element(AE, Set)}.
priv_add_called(AE_, State=#{called := Set}) ->
    AE = reshape(AE_),
    State#{allow_all => false, called => sets:add_element(AE, Set)}.

check(CallingAE_, CalledAE_, #{both := Set}) ->
    CallingAE = reshape(CallingAE_),
    CalledAE = reshape(CalledAE_),
    sets:is_element({CallingAE, CalledAE}, Set).
check_calling(AE_, #{calling := Set}) ->
    AE = reshape(AE_),
    sets:is_element(AE, Set).
check_called(AE_, #{called := Set}) ->
    AE = reshape(AE_),
    sets:is_element(AE, Set).

allowed(CallingAE, CalledAE, State) ->
    check(CallingAE, CalledAE, State) or
	check_calling(CallingAE, State) or
	check_called(CalledAE, State).

init_state(missing, missing) ->
    default();
init_state(missing, CalledAE) ->
    priv_add_called(CalledAE, default());
init_state(CallingAE, missing) ->
    priv_add_calling(CallingAE, default());
init_state(CallingAE, CalledAE) ->
    priv_add(CallingAE, CalledAE, default()).

%%------------------------------------------------------------------------------
%% Support
%%------------------------------------------------------------------------------

binary_env_var(Name, Default) when is_list(Default) ->
    binary_env_var(Name, list_to_binary(Default));

binary_env_var(Name, Default) ->
    case os:getenv(Name) of
	false ->
	    Default;
	Var ->
	    list_to_binary(Var)
    end.

reshape(AE) when is_list(AE) ->
    reshape(list_to_binary(AE));
reshape(AE) ->
    wolfpacs_vr_utils:trim_binary(AE).

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

env_test_() ->
    Home = binary_env_var("HOME", ""),
    [ ?_assert(byte_size(Home) > 0)
    , ?_assertEqual(binary_env_var("SFASKELJALKJELKFKLJAFDAEEAKJIIEFA", missing), missing)
    ].

minimal_both_calling_and_called__test_() ->
    _ = start_link(),
    [ ?_assertEqual(authenticate("A", "B"), {ok, true}) %% all are allowed
    , ?_assertEqual(authenticate("C", "D"), {ok, true}) %% all are allowed
    , ?_assertEqual(add("A", "B"), ok)
    , ?_assertEqual(authenticate("A", "B"), {ok, true})
    , ?_assertEqual(authenticate("C", "D"), {ok, false})
    , ?_assertEqual(stop(), ok)
    ].

minimal_calling_test_() ->
    _ = start_link(),
    [ ?_assertEqual(authenticate("A", "B"), {ok, true}) %% all are allowed
    , ?_assertEqual(authenticate("C", "D"), {ok, true}) %% all are allowed
    , ?_assertEqual(add_calling("A"), ok)
    , ?_assertEqual(authenticate("A", "B"), {ok, true})
    , ?_assertEqual(authenticate("A", "_"), {ok, true})
    , ?_assertEqual(authenticate("C", "D"), {ok, false})
    , ?_assertEqual(stop(), ok)
    ].

minimal_called_test_() ->
    _ = start_link(),
    [ ?_assertEqual(authenticate("A", "B"), {ok, true}) %% all are allowed
    , ?_assertEqual(authenticate("C", "D"), {ok, true}) %% all are allowed
    , ?_assertEqual(add_called("B"), ok)
    , ?_assertEqual(authenticate("A", "B"), {ok, true})
    , ?_assertEqual(authenticate("_", "B"), {ok, true})
    , ?_assertEqual(authenticate("C", "D"), {ok, false})
    , ?_assertEqual(stop(), ok)
    ].
