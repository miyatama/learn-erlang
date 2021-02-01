-module(motor_controller).

-export([add_event_handler/0]).

-define(DEBUG(S), io:fwrite("[DEBUG] " ++ S ++ "~n")).

add_event_handler() ->
	?DEBUG("add_event_handler/0"),
	event_handler:add_handler(errors, fun controller/1).

controller(too_hot) ->
	?DEBUG("controller/1"),
	io:format("turn off the motor~n");
controller(X) ->
	?DEBUG("controller/1"),
	io:format("~p ignored event: ~p~n", [?MODULE, X]).
