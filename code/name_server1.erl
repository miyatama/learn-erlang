-module(name_server1).
-export([init/0,
	 add/2,
	 whereis/1,
	 handle/2]).
-import(server3, [rpc/2]).

-define(DEBUG(S), io:fwrite("[DEBUG] name_server1: " ++ S ++ "~n")).

add(Name, Place) ->
	?DEBUG("add/2"),
	rpc(name_server, {add, Name, Place}).

whereis(Name) ->
	?DEBUG("whereis/1"),
	rpc(name_server, {whereis, Name}).

init() -> 
	?DEBUG("init/0"),
	dict:new().

handle({add, Name, Place}, Dict) ->
	?DEBUG("handle/2"),
	{ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) ->
	?DEBUG("handle/2"),
	{dict:find(Name, Dict), Dict}.
