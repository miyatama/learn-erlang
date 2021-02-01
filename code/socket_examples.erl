-module(socket_examples).
% -compile(export_all).
-export([nano_get_url/0,
	nano_get_url/1,
	nano_client_eval/1,
	start_nano_server/0,
	error_test/0,
	error_test_server/0,
	start_seq_server/0,
	start_parallel_server/0]).
-import(lists, [reverse/1]).
-define(DEBUG(S), io:fwrite("[DEBUG] socket_examples: " ++ S ++ "~n")).

nano_get_url() ->
    ?DEBUG("nano_get_url/0"),
    nano_get_url("www.google.com").

nano_get_url(Host) ->
    ?DEBUG("nano_get_url/1 - Host: " ++ Host),
    {ok,Socket} = gen_tcp:connect(Host,80,[binary, {packet, 0}]), %% <callout id="nanoget.url.1"/>
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),  %% <callout id="nanoget.url.2"/>
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    ?DEBUG("receive_data/2"),
    receive
	{tcp,Socket,Bin} ->    %% <callout id="nanoget.url.3"/>
	    receive_data(Socket, [Bin|SoFar]);
	{tcp_closed,Socket} -> %% <callout id="nanoget.url.4"/>
	    list_to_binary(reverse(SoFar)) %% <callout id="nanoget.url.5"/>
    end.

start_seq_server() ->
    	?DEBUG("start_seq_server/0"),
	{ok, Listen} = gen_tcp:listen(2345, 
					[binary, 
					{packet, 4},
					{reuseaddr, true},
					{active, true}]),
	seq_loop(Listen).

seq_loop(Listen) ->
    	?DEBUG("seq_loop/0"),
	{ok, Socket} = gen_tcp:accept(Listen),
	loop(Socket),
	seq_loop(Listen).

start_parallel_server() ->
    	?DEBUG("start_parallel_server/0"),
	{ok, Listen} = gen_tcp:listen(2345, 
					[binary, 
					{packet, 4},
					{reuseaddr, true},
					{active, true}]),
	spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    	?DEBUG("par_connect/1"),
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> par_connect(Listen) end),
	loop(Socket).


nano_client_eval(Str) ->
    ?DEBUG("nano_client_eval/1"),
    {ok, Socket} = 
	gen_tcp:connect("localhost", 2345,
			[binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
	{tcp,Socket,Bin} ->
	    io:format("Client received binary = ~p~n",[Bin]),
	    Val = binary_to_term(Bin),
	    io:format("Client result = ~p~n",[Val]),
	    gen_tcp:close(Socket)
    end.

start_nano_server() ->
    ?DEBUG("start_nano_server/0"),
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},  %% <callout id="nanos.1"/>
					 {reuseaddr, true},
					 {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),  %% <callout id="nanos.1a"/>
    gen_tcp:close(Listen),  %% <callout id="nanos.1b"/>
    loop(Socket).

loop(Socket) ->
    ?DEBUG("loop/1"),
    receive
	{tcp, Socket, Bin} ->
	    io:format("Server received binary = ~p~n",[Bin]),
	    Str = binary_to_term(Bin),  %% <callout id="nanos.2"/>
	    io:format("Server (unpacked)  ~p~n",[Str]),
	    Reply = lib_misc:string2value(Str),  %% <callout id="nanos.3"/>
	    io:format("Server replying = ~p~n",[Reply]),
	    gen_tcp:send(Socket, term_to_binary(Reply)),  %% <callout id="nanos.4"/>
	    loop(Socket);
	{tcp_closed, Socket} ->
	    io:format("Server socket closed~n")
    end.

error_test() ->
    ?DEBUG("error_test/0"),
    spawn(fun() -> error_test_server() end),
    lib_misc:sleep(2000),
    {ok,Socket} = gen_tcp:connect("localhost",4321,[binary, {packet, 2}]),
    io:format("connected to:~p~n",[Socket]),
    gen_tcp:send(Socket, <<"123">>),
    receive
	Any ->
	    io:format("Any=~p~n",[Any])
    end.

error_test_server() ->
    ?DEBUG("error_test_server/0"),
    {ok, Listen} = gen_tcp:listen(4321, [binary,{packet,2}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    error_test_server_loop(Socket).

error_test_server_loop(Socket) ->
    ?DEBUG("error_test_server_loop/1"),
    receive
	{tcp, Socket, Data} ->
	    io:format("received:~p~n",[Data]),
	    List = atom_to_list(Data),
	    show_list(List),
	    error_test_server_loop(Socket)
    end.

show_list([H|T]) ->
	io:format("show_list: ~p~n", [H]),
	show_list(T);
show_list([]) ->
	true.
