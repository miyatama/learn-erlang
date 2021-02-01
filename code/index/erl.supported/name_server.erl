-module(name_server).
-export([init/0,
	add/2,
	whereis/1,
	handle/2,
	add2/2,
	whereis2/1,
	add3/2,
	whereis3/1]).
-import(server1, 
	[rpc/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] name_server: " ++ S ++ "~n")).

init() ->
	?DEBUG("init/0"),
	dict:new().

add(Name, Place) ->
	?DEBUG("add/2"),
	rpc(name_server, {add, Name, Place}).

whereis(Name) ->
	?DEBUG("whereis/1"),
	rpc(name_server, {whereis, Name}).

add2(Name, Place) ->
	?DEBUG("add2/2"),
	server2:rpc(name_server, {add, Name, Place}).

whereis2(Name) ->
	?DEBUG("whereis2/1"),
	server2:rpc(name_server, {whereis, Name}).

add3(Name, Place) ->
	?DEBUG("add3/2"),
	server3:rpc(name_server, {add, Name, Place}).

whereis3(Name) ->
	?DEBUG("whereis3/1"),
	server3:rpc(name_server, {whereis, Name}).

handle({add, Name, Place}, Dict) ->
	?DEBUG("handle/2"),
	{ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) ->
	?DEBUG("handle/2"),
	{dict:find(Name, Dict), Dict}.


