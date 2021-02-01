-module(sellaprime_app).
-behavior(application).
-export([start/2,
	stop/1]).

-define(DEBUG(S), io:fwrite("[DEBUG] sellaprime_app: " ++ S ++ "~n")).

start(_Type, StartArgs) ->
	?DEBUG("start/2"),
	sellaprime_supervisor:start_link(StartArgs).

stop(_State) ->
	?DEBUG("stop/1"),
	ok.
