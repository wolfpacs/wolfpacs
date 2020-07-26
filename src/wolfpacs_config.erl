-module(wolfpacs_config).
-export([load/0,
	 load_config_file/1]).

load() ->
    spawn(fun() ->
		  timer:sleep(2000),
		  load_config_in_folder(os:getenv("WOLFPACS_DIR"))
	  end),
    ok.

load_config_in_folder(false) ->
    _ = lager:warning("[Config] No config file specified");
load_config_in_folder(Folder) ->
    Filename = filename:join(Folder, "wolfpacs.conf"),
    load_config_file(Filename).

load_config_file(Filename) ->
    load_config_content(file:consult(Filename)).

load_config_content({ok, Terms}) ->
    load_terms(Terms);
load_config_content({error, enoent}) ->
    lager:warning("[Config] Config file named but missing"),
    error;
load_config_content({error, Reason}) ->
    lager:warning("[Config] Config error ~p", [Reason]),
    error.

load_terms([]) ->
    ok;
load_terms([{worker, Host, Port, AE}|Terms]) ->
    wolfpacs_outside_router:add_worker(Host, Port, AE),
    load_terms(Terms);
load_terms([{destination, Ref, Host, Port}|Terms]) ->
    wolfpacs_inside_router:set_destination(Ref, Host, Port),
    load_terms(Terms);
load_terms([Item|_Terms]) ->
    _ = lager:warning("[Config] Don't understand ~p", [Item]),
    error.
