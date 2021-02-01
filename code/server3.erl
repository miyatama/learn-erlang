-module(server3).
-export([start/2,
	 rpc/2,
	 swap_code/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] server3: " ++ S ++ "~n")).

start(Name, Mod) ->
	?DEBUG("start/2"),
	register(Name,
		 spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) ->
	?DEBUG("swap_code/2"),
	rpc(Name, {swap_code, Mod}).


rpc(Name, Request) ->
	?DEBUG("rpc/2"),
	Name ! {self(), Request},
	receive
		{Name, Response} -> Response
	end.

loop(Name, Mod, OldState) ->
	?DEBUG("loop/3"),
	receive
		{From, {swap_code, NewCallbackMod}} ->
			From ! {Name, ack},
			loop(Name, NewCallbackMod, OldState);
		{From, Request} ->
			{Response, NewState} = Mod:handle(Request, OldState),
			From ! {Name, Response},
			loop(Name, Mod, NewState)
	end.
