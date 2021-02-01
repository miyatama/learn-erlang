-module(server5).
-export([start/0,
	 rpc/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] server5: " ++ S ++ "~n")).

start() ->
	?DEBUG("start/0"),
	spawn(fun() -> wait() end).

rpc(Pid, Q) ->
	?DEBUG("rpc/2"),
	Pid ! {self(), Q},
	receive
		{Pid, Reply} -> Reply
	end.

wait() ->
	receive
		{become, F} -> F()
	end.

