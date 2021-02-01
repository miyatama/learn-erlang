-module(lib_chan_mm).
-export([loop/2,
	 send/2,
	 close/1,
	 controller/2,
	 set_trace/2,
	 trace_with_tag/2]).
-define(DEBUG(Str), io:fwrite("[DEBUG] lib_chan_mm:" ++ Str ++ "~n")).

send(Pid, Term) -> Pid ! {send, Term}.

close(Pid) -> Pid ! close.

controller(Pid, Pid1) -> Pid ! {setController, Pid1}.

set_trace(Pid, X) -> Pid ! {trace, X}.

trace_with_tag(Pid, Tag) ->
	set_trace(Pid, {true, fun(Msg) -> io:format("MM: ~p ~p~n", [Tag, Msg]) end}).

loop(Socket, Pid) ->
	?DEBUG("loop/2"),
	process_flag(trap_exit, true),
	loop1(Socket, Pid, false).

loop1(Socket, Pid, Trace) ->
	?DEBUG("loop1/3"),
	receive
		{tcp, Socket, Bin}->
			?DEBUG("loop1/3 - {tcp, _, _}"),
			Term = binary_to_term(Bin),
			trace_it(Trace, {socketReceived, Term}),
			Pid ! {chan, self(), Term},
			loop1(Socket, Pid, Trace);
		{tcp_closed, Socket}->
			?DEBUG("loop1/3 - {tcp_closed, _}"),
			trace_it(Trace, socketClosed),
			Pid ! {chan_closed, self()};
		{'EXIT', Pid, Why}->
			?DEBUG("loop1/3 - {'EXIT', _, _}"),
			trace_it(Trace, {controllingProcessExit, Why}),
			gen_tcp:close(Socket);
		{setController, Pid1}->
			?DEBUG("loop1/3 - {setController, _}"),
			trace_it(Trace, {changedController, Pid}),
			loop1(Socket, Pid1, Trace);
		{trace, Trace1}->
			?DEBUG("loop1/3 - {trace, _}"),
			trace_it(Trace, {setTrace, Trace1}),
			loop1(Socket, Pid, Trace1);
		close->
			?DEBUG("loop1/3 - close"),
			trace_it(Trace, closedByClient),
			gen_tcp:close(Socket);
		{send, Term}->
			?DEBUG("loop1/3 - {send, _}"),
			trace_it(Trace, {sendingMessage, Term}),
			gen_tcp:send(Socket, term_to_binary(Term)),
			loop1(Socket, Pid, Trace);
		UUg->
			?DEBUG("loop1/3 - UUg"),
			io:format("lib_chan_mm: protocol error: ~p~n", [UUg]),
			loop1(Socket, Pid, Trace)
	end.

trace_it(false, _) -> void;
trace_it({true, F}, M) -> F(M).
