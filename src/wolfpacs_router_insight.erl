-module(wolfpacs_router_insight).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 stop/0,
	 note/6]).

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

note(RouteTag, CalledAE, CallingAE, ImageType, StudyUID, SeriesUID) ->
    gen_server:cast(?MODULE, {note, RouteTag, CalledAE, CallingAE, ImageType, StudyUID, SeriesUID}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init(_) ->
    {ok, []}.

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(Event={note, _, _, _, _, _, _}, Events) ->
    print_if_unseen(Event, Events),
    {noreply, [Event|Events]};

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

print_if_unseen(Event, Events) ->
    P = fun(E) -> E =:= Event end,
    case lists:filter(P, Events) of
	[] ->
	    print(Event);
	_ ->
	    ok
    end.

print({note, RouteTag, _CalledAE, _CallingAE, ImageType, StudyUID, _SeriesUID}) ->
    print_dir(RouteTag, StudyUID, ImageType);
print(Event) ->
    lager:warning("[RouteInsight] Unable to understand ~p", [Event]),
    ok.

print_dir(wolfpacs_outside, _StudyUID, ImageType) ->
    io:fwrite(" --> ~s~n", [human(ImageType)]);

print_dir(wolfpacs_inside, _StudyUID, ImageType) ->
    io:fwrite(" <-- ~s~n", [human(ImageType)]);

print_dir(RouteTag, StudyUID, ImageType) ->
    io:fwrite("[RouterInsight] [~p] [~p] ~p", [RouteTag, StudyUID, ImageType]).

human(BinaryText) ->
    Text = binary_to_list(BinaryText),
    string:join(string:tokens(Text, "\\"), " ").

%%==============================================================================
%% Test
%%==============================================================================

-include_lib("eunit/include/eunit.hrl").

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
