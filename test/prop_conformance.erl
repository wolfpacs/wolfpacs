-module(prop_conformance).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(PCS_, presentation_contextes(),
	    begin
		PCS = add_id(PCS_),
		{ok, Items, Map} = wolfpacs_conformance:supported(PCS),
		check_all(Items, Map)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
add_id(PresentationContextes) ->
    add_id(PresentationContextes, 1, []).

add_id([], _, Acc) ->
    lists:reverse(Acc);
add_id([{A, T}|Rest], N, Acc) ->
    add_id(Rest, N+1, [{N, A, T}|Acc]).

check_all([], _) ->
    true;
check_all([{Id,T}|Rest], Map) ->
    S = wolfpacs_conformance:transfer_syntax_to_strategy(T),
    case maps:get(Id, Map, missing) of
	missing ->
	    false;
	{_, S} ->
	    check_all(Rest, Map);
	_  ->
	    false
    end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

abstract_syntax() ->
    oneof([ <<"1.2.840.10008.1.1">>
	  , <<"1.2.840.10008.5.1.4.1.1.7">>
	  , <<"1.2.840.10008.5.1.4.1.1.2">>
	  , <<"1.2.840.10008.5.1.4.1.1.4">>
	  ]).

transfer_syntax() ->
    oneof([ <<"1.2.840.10008.1.2">>
	  , <<"1.2.840.10008.1.2.1">>
	  , <<"1.2.840.10008.1.2.2">>
	  ]).

transfer_syntaxes() ->
    non_empty(list(transfer_syntax())).

presentation_context() ->
    {abstract_syntax(), transfer_syntaxes()}.

presentation_contextes() ->
    non_empty(list(presentation_context())).
