-module(server1).
-export([start/2,
	 rpc/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] server1: " ++ S ++ "~n")).

start(Name, Mod) ->
	?DEBUG("start/2"),
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
	?DEBUG("rpc/2"),
	Name ! {self(), Request},
	receive
		{Name, Response} ->
			Response
	end.

loop(Name, Mod, State) ->
	?DEBUG("loop/3"),
	receive
		{From, Request} ->
			{Response, State1} = Mod:handle(Request, State),
			From ! {Name, Response},
			loop(Name, Mod, State1)
	end.
