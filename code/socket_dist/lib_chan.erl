-module(lib_chan).
-export(
   [cast/2,
    start_server/0,
    start_server/1,
    connect/5,
    disconnect/1,
    rpc/2]).
-import(lists, [map/2, member/2, foreach/2]).
-import(lib_chan_mm, [send/2, close/1]).
-define(DEBUG(Str), io:fwrite("[DEBUG] lib_chan:" ++ Str ++ "~n")).

% for server
start_server() ->
	?DEBUG("start_server/0"),
	case os:getenv("HOME") of
		false ->
			exit({ebadEnv, "HOME"});
		Home ->
			start_server(Home ++ "/.erlang_config/lib_chan.conf")
	end.

start_server(ConfigFile) ->
	?DEBUG("start_server/1"),
	io:format("lib_chan starting: ~p~n", [ConfigFile]),
	case file:consult(ConfigFile) of
		{ok, ConfigData} ->
			io:format("ConfigData=~p~n", [ConfigData]),
			case check_terms(ConfigData) of
				[] ->
					start_server1(ConfigData);
				Errors ->
					exit({eDeamonConfig, Errors})
			end;
		{error, Why} ->
			exit({eDeamonConfig, Why})

	end.

check_terms(ConfigData) ->
	?DEBUG("check_terms/1"),
	L = map(fun check_term/1, ConfigData),
	[X || {error, X} <- L].

check_term({port, P}) when is_integer(P) -> 
	?DEBUG("check_term/1"),
	ok;
check_term({service, _, password, _, mfa, _, _, _}) -> 
	?DEBUG("check_term/1"),
	ok;
check_term(X) -> 
	?DEBUG("check_term/1"),
	{error, {badTerm, X}}.

start_server1(ConfigData) ->
	?DEBUG("start_server1/1"),
	register(lib_chan, spawn(fun() -> start_server2(ConfigData) end)).

start_server2(ConfigData) ->
	?DEBUG("start_server2/1"),
	[Port] = [P || {port, P} <- ConfigData],
	start_port_server(Port, ConfigData).

start_port_server(Port, ConfigData) -> 
	?DEBUG("start_port_server/2"),
	lib_chan_cs:start_raw_server(
		Port,
		fun(Socket) -> start_port_instance(Socket, ConfigData) end,
		100,
		4).

start_port_instance(Socket, ConfigData) -> 
	?DEBUG("start_port_instance/2"),
	S = self(),
	Controller = spawn_link(fun() -> start_erl_port_service(S, ConfigData) end),
	lib_chan_mm:loop(Socket, Controller).

start_erl_port_service(MM, ConfigData) ->
	?DEBUG("start_erl_port_service/2"),
	receive
		{chan, MM, {startService, Mod, ArgC}} ->
			?DEBUG("start_erl_port_service/2 - {chan, _ {}}"),
			case get_service_definition(Mod, ConfigData) of
				{yes, Pwd, MFA} ->
					case Pwd of
						none ->
							send(MM, ack),
							really_start(MM, ArgC, MFA);
						_ -> 
							do_authentication(Pwd, MM, ArgC, MFA)
					end;
				no ->
					io:format("sending bad service~n",[]),
					send(MM, badService),
					close(MM)
			end;
		Any ->
			io:format("*** Erl port server got: ~p ~p~n", [MM, Any]),
			exit({protocolViolation, Any})
	end.

do_authentication(Pwd, MM, ArgC, MFA) ->
	?DEBUG("do_authentication/4"),
	C = lib_chan_auth:make_challenge(),
	send(MM, {challenge, C}),
	receive
		{chan, MM, {response, R}} ->
			?DEBUG("do_authentication/4 - {chan, _ {response, _}}"),
			case lib_chan_auth:is_response_correct(C, R, Pwd) of
				true -> 
					send(MM, ack),
					really_start(MM, ArgC, MFA);
				false ->
					send(MM, authFail),
					close(MM)
			end
	end.


really_start(MM, ArgC, {Mod, Func, ArgS}) ->
	?DEBUG("really_start/3"),
	case (catch apply(Mod, Func, [MM, ArgC, ArgS])) of
	  {'EXIT', normal} ->
	  	true;
	  {'EXIT', Why} ->
		io:format("server error: ~p~n", [Why]);
	  Why ->
	  	io:format("server error should die with exit(normal) was: ~p~n", [Why])
	end.

get_service_definition(Mod, [{service, Mod, password, Pwd, mfa, M, F, A} | _]) ->
	?DEBUG("get_service_definition/1"),
	{yes, Pwd, {M, F, A}};
get_service_definition(Name, [_ | T]) -> 
	?DEBUG("get_service_definition/2"),
	get_service_definition(Name, T);
get_service_definition(_, []) ->
	?DEBUG("get_service_definition/2"),
	no.

% for client
connect(Host, Port, Service, Secret, ArgC) ->
	?DEBUG("connect/5"),
	S = self(),
	MM = spawn(fun() -> connect(S, Host, Port) end),
	receive
		{MM, ok} ->
			?DEBUG("connect/5 - {_, ok}"),
			case authenticate(MM, Service, Secret, ArgC) of
				ok -> 
					?DEBUG("connect/5 - authenticate - ok"),
					{ok, MM};
				Error -> 
					?DEBUG("connect/5 - authenticate - Error"),
					Error
			end;
		{MM, Error} ->
			Error
	end.

connect(Parent, Host, Port) ->
	?DEBUG("connect/3"),
	case lib_chan_cs:start_raw_client(Host, Port, 4) of
		{ok,Socket} ->
			?DEBUG("connect/3 - {ok, _}"),
			Parent ! {self(), ok},
			lib_chan_mm:loop(Socket, Parent);
		Error ->
			?DEBUG("connect/3 - Error"),
			Parent ! {self(), Error}
	end.

authenticate(MM, Service, Secret, ArgC) ->
	?DEBUG("authenticate/4"),
	send(MM, {startService, Service, ArgC}),
	receive
		{chan, MM, ack} ->
			?DEBUG("authenticate/4 - {chan, _, ack}"),
			ok;
		{chan, MM, {challenge, C}} ->
			?DEBUG("authenticate/4 - {chan, _, {challenge, _}}"),
			R = lib_chan_auth:make_response(C, Secret),
			send(MM, {response, R}),
			?DEBUG("authenticate/4 - wait after send response"),
			receive
				{chan, MM, ack} ->
					?DEBUG("authenticate/4 - {chan, _ ack}"),
					ok;
				{chan, MM, authFail} ->
					?DEBUG("authenticate/4 - {chan, _ authFail}"),
					wait_close(MM),
					{error, authFail};
				Other ->
					?DEBUG("authenticate/4 - Other"),
					{error, Other}
			end;
		{chan, MM, badService} ->
			?DEBUG("authenticate/4 - {chan, _, badService}"),
			wait_close(MM),
			{error, badService};
		Other ->
			?DEBUG("authenticate/4 - Other"),
			{error, Other}
	end.

wait_close(MM) ->
	?DEBUG("wait_close/1"),
	receive
		{chan_closed, MM} ->
			true
	after 5000 ->
		      io:format("*** error lib_chan~n", []),
		      true
	end.

disconnect(MM) -> 
	?DEBUG("disconnect/1"),
	close(MM).

rpc(MM, Q) ->
	?DEBUG("rpc/2"),
	send(MM, Q),
	receive
		{chan, MM, Reply} ->
			?DEBUG("rpc/2 - {chan, _, _}"),
			Reply
	end.

cast(MM, Q) ->
	?DEBUG("cast/2"),
	send(MM, Q).
