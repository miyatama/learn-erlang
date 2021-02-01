-module(chat_server).
-import(lib_chan_mm, 
	[send/2,
	 contorller/2]).
-import(lists,
	[delete/2,
	 foreach/2,
	 map/2,
	 member/2,
	 reverse/2]).
% -compile(export_all).
-export([start/0]).

-define(DEBUG(S), io:fwrite("[DEBUG] chat_server:" ++ S ++ "~n")).

start() ->
	?DEBUG("start/0"),
	start_server(),
	lib_chan:start_server("chat.conf").

start_server() ->
	?DEBUG("start_server/0"),
	register(chat_server,
		spawn(fun() ->
			process_flag(trap_exit, true),
			Val = (catch server_loop([])),
			io:format("server terminated with: ~p~n", [Val]) end)).

server_loop(L) ->
	?DEBUG("server_loop/0"),
	receive
		{mm, Channel,{login, Group, Nick}} ->
			case lookup(Group, L) of
				{ok, Pid} ->
					Pid ! {login, Channel, Nick},
					server_loop(L);
				error ->
					Pid = spawn_link(
						fun() -> chat_group:start(Channel, Nick) end),
					server_loop([{Group, Pid}|L])
			end;
		{mm_closed, _} ->
			server_loop(L);
		{'EXIT', Pid, allGone} ->
			L1 = remove_group(Pid, L),
			server_loop(L1);
		Msg ->
			io:format("server received msg: ~p~n", [Msg]),
			server_loop(L)
	end.

lookup(G, [{G, Pid}|_]) ->
	?DEBUG("lookup/2"),
	{ok, Pid};
lookup(G, [_|T]) ->
	?DEBUG("lookup/2"),
	lookup(G, T);
lookup(_, []) ->
	?DEBUG("lookup/2"),
	error.

remove_group(Pid, [{G, Pid}|T]) ->
	?DEBUG("remove_group/2"),
	io:format("~p removed~n", [G]), 
	T;
remove_group(Pid, [H|T]) ->
	?DEBUG("remove_group/2"),
	[H|remove_group(Pid, T)];
remove_group(_, []) ->
	?DEBUG("remove_group/2"),
	[].
