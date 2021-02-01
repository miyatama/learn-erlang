-module(new_name_server).
-export([init/0,
	 add/2,
	 all_names/0,
	 delete/1,
	 whereis/1,
	 handle/2]).
-import(server3, [rpc/2]).
-define(DEBUG(S), io:fwrite("[DEBUG] new_name_server: " ++ S ++ "~n")).

all_names() ->
      ?DEBUG("all_names/0"),
      rpc(name_server, allNames).

add(Name, Place) ->
	?DEBUG("add/2"),
	rpc(name_server, {add, Name, Place}).

delete(Name) ->
	?DEBUG("delete/1"),
	rpc(name_server, {delete, Name}).

whereis(Name) ->
	?DEBUG("whereis/1"),
	rpc(name_server, {whereis, Name}).

init() ->
	?DEBUG("init/0"),
	dict:new().

handle({add, Name, Place}, Dict) ->
	?DEBUG("handle/2"),
	{ok, dict:store(Name, Place, Dict)};
handle({delete, Name}, Dict) ->
	?DEBUG("handle/2"),
	{ok, dict:erase(Name, Dict), Dict};
handle({whereis, Name}, Dict) ->
	?DEBUG("handle/2"),
	{dict:find(Name, Dict), Dict};
handle(allNames, Dict) ->
	?DEBUG("handle/2"),
	{dict:fetch_keys(Dict), Dict}.
