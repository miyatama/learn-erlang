-module(server2).
-export([start/2,
	 rpc/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] server2: " ++ S ++ "~n")).

start(Name, Mod) ->
	?DEBUG("start/2"),
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
	?DEBUG("rpc/2"),
	Name ! {self(), Request},
	receive
		{Name, crash} -> exit(rpc);
		{Name, ok, Response} -> Response
	end.

loop(Name, Mod, OldState) ->
	?DEBUG("loop/3"),
	receive
		{From, Request} ->
			try Mod:handle(Request, OldState) of
				{Response, NewState} ->
					From ! {Name, ok, Response},
					loop(Name, Mod, NewState)
			catch
				_:Why ->
					log_the_error(Name, Request, Why),
					From ! {Name, crash},
					loop(Name, Mod, OldState)
			end
	end.

log_the_error(Name, Request, Why) ->
	?DEBUG("log_the_error/3"),
	io:format("server ~p request ~p ~ncaused exception ~p~n", [Name, Request, Why]).
