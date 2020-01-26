-module(prop_random_tcp_data).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Blob, binary(),
        begin
	    application:ensure_all_started(wolfpacs),

	    Hostname = "localhost",
	    Port = 11112,

	    %% Send some random garbage data to wolfpacs
	    {ok, Sock} = gen_tcp:connect(Hostname, Port, [binary, {active, false}]),
	    ok = gen_tcp:send(Sock, Blob),
	    ok = timer:sleep(10),

	    %% Make sure that wolfpacs is still running and responding to echos
	    Strategy = {implicit, little},
	    {ok, Echo} = wolfpacs_c_echo_scu:start_link(),
	    {ok, success} = wolfpacs_c_echo_scu:echo(Echo, Hostname, Port, <<"testtest">>, Strategy),
	    ok =:= wolfpacs_c_echo_scu:stop(Echo)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
