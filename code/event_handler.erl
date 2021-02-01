-module(event_handler).
-export([make/1,
	 add_handler/2,
	 event/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] event_handler: " ++ S ++ "~n")).

make(Name) ->
	?DEBUG("make/1"),
	register(Name, spawn(fun() -> my_handler(fun no_op/1) end)).

add_handler(Name, Fun) ->
	?DEBUG("add_handler/2"),
	Name ! {add, Fun}.

event(Name, X) ->
	?DEBUG("event/2"),
	Name ! {event, X}.

my_handler(Fun) ->
	?DEBUG("my_handler/2"),
	receive
		{add, Fun1} ->
			my_handler(Fun1);
		{event, Any} ->
			(catch Fun(Any)),
			my_handler(Fun)
	end.

no_op(_) -> 
	?DEBUG("no_op/2"),
	void.
