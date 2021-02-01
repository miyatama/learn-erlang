-module(lib_chan_cs).
-export(
   [start_raw_server/4,
    start_raw_client/3]).
-export([stop/1]).
-export([children/1]).
-define(DEBUG(Str), io:fwrite("[DEBUG] lib_chan_cs:" ++ Str ++ "~n")).

start_raw_client(Host, Port, PacketLength) ->
	?DEBUG("start_raw_client/3"),
	io:fwrite("lib_chan_cs:start_raw_client/3~n"),
	gen_tcp:connect(
		Host, 
		Port, 
		[binary, 
		 {active, true}, 
		 {packet, PacketLength}]).

start_raw_server(Port, Fun, Max, PacketLength) ->
	?DEBUG("start_raw_server/4"),
	Name = port_name(Port),
	case whereis(Name) of
		undefined ->
			Self = self(),
			Pid = spawn_link(fun() -> cold_start(Self, Port, Fun, Max, PacketLength) end),
			receive
				{Pid, ok} ->
					register(Name, Pid),
					{ok, self()};
				{Pid, Error} ->
					Error
			end;
		_Pid ->
			{error, already_started}
	end.

stop(Port) when is_integer(Port) ->
	?DEBUG("stop/1"),
	Name = port_name(Port),
	case whereis(Name) of
		undefined ->
			not_started;
		Pid ->
			exit(Pid, kill),
			(catch unregister(Name)),
			stopped

	end.

children(Port) when is_integer(Port) ->
	?DEBUG("children/1"),
	port_name(Port) ! {children, self()},
	receive
		{session_server, Reply} ->
			Reply
	end.

port_name(Port) when is_integer(Port) ->
	?DEBUG("port_name/1"),
	list_to_atom("portServer" ++ integer_to_list(Port)).


cold_start(Master, Port, Fun, Max, PacketLength) ->
	?DEBUG("cold_start/5"),
	process_flag(trap_exit, true),
	case gen_tcp:listen(
	       Port, 
	       [binary,
		% {dontroute, true},
		{nodelay, true},
		{packet, PacketLength},
		{reuseaddr, true},
		{active, true}]) of
		{ok, Listen} ->
			Master ! {self(), ok},
			New = start_accept(Listen, Fun),
			socket_loop(Listen, New, [], Fun, Max);
		Error ->
			Master ! {self(), Error}
	end.

socket_loop(Listen, New, Active, Fun, Max) ->
	?DEBUG("socket_loop/5"),
	receive
		{istarted, New} ->
			Active1 = [New|Active],
			possibly_start_another(false, Listen, Active1, Fun, Max);
		{'EXIT', New, Why} ->
			io:format("Child exit: ~p~n", [Why]),
			possibly_start_another(false, Listen, Active, Fun, Max);
		{'EXIT', Pid, Why} ->
			io:format("Child exit: ~p~n", [Why]),
			Active1 = lists:delete(Pid, Active),
			possibly_start_another(New, Listen, Active1, Fun, Max);
		{children, From} ->
			From ! {session_server, Active},
			socket_loop(Listen, New, Active, Fun, Max);
		_Other ->
			socket_loop(Listen, New, Active, Fun, Max)
	end.

possibly_start_another(New, Listen, Active, Fun, Max) when is_pid(New) ->
	?DEBUG("possibly_start_another/5"),
	socket_loop(Listen, New, Active, Fun, Max);
possibly_start_another(false, Listen, Active, Fun, Max) ->
	?DEBUG("possibly_start_another/5"),
	case length(Active) of
		N when N < Max ->
			New = start_accept(Listen, Fun),
			socket_loop(Listen, New, Active, Fun, Max);
		_ -> 
			socket_loop(Listen, false, Active, Fun, Max)
	end.

start_accept(Listen, Fun) ->
	?DEBUG("start_accept/2"),
	S = self(),
	spawn_link(fun() -> start_child(S, Listen, Fun) end).

start_child(Parent, Listen, Fun) ->
	?DEBUG("start_child/3"),
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			Parent ! {istarted, self()},
			inet:setopts(
			  Socket,
			  [{packet, 4},
			   binary,
			   {nodelay, true},
			   {active, true}]),
			process_flag(trap_exit, true),
			case (catch Fun(Socket)) of
				{'EXIT', normal} ->
					true;
				{'EXIT', Why} ->
					io:format("Port process ides with exit: ~p~n", [Why]),
					true;
				_ ->
					true
			end
	end.
