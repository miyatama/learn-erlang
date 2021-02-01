-module(my_alarm_handler).
-behavior(gen_event).

-export([init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	termiinate/2]).

init(Args) ->
	io:format("*** my_alarm_handler init:~p~n", [Args]),
	{ok, 0}.

handle_event({set_alarm, too_hot}, N) ->
	error_logger:error_msg("*** tell the engineer to turn on the fan~n"),
	{ok, N+1};
handle_event({clear_alarm, too_hot}, N) ->
	error_logger:error_msg("*** danger over. turn off the fan~n"),
	{ok, N};
handle_event(Event, N) ->
	io:format("*** unmatched event: ~p~n", [Event]),
	{ok, N}.

handle_call(_Request, N) ->
	Reply = N,
	{ok, Reply, N}.

handle_info(_Info, N) ->
	{ok, N}.

termiinate(_Reason, _N) ->  
	ok.
