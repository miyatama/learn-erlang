-module(server4).
-export([start/2,
	 rpc/2,
	 swap_code/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] server4: " ++ S ++ "~n")).

start(Name, Mod) ->
	?DEBUG("strat/2"),
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) ->
	?DEBUG("swap_code/2"),
	rpc(Name, {swap_code, Mod}).

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
		{From, {swap_code, NewCallbackModule}} ->
			From ! {Name, ok, ack},
			loop(Name, NewCallbackModule, OldState);
		{From, Request} ->
			try Mod:handle(Request, OldState) of
				{Response, NewState} ->
					From ! {Name, ok, Response },
					loop(Name, Mod, NewState)
			catch
				_: Why ->
					log_the_error(Name, Request, Why),
					From ! {Name, crash},
					loop(Name, Mod, OldState)
			end
	end.

log_the_error(Name, Request, Why) ->
	?DEBUG("log_the_error/3"),
	io:format("Server ~p request ~p ~ncaused exception ~p~n",
		 [Name, Request, Why]).

