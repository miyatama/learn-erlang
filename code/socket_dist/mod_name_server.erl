-module(mod_name_server).
-export([start_me_up/3]).
-define(DEBUG(Str), io:fwrite("[DEBUG] mod_name_server:" ++ Str ++ "~n")).

start_me_up(MM, _ArgC, _ArgS) ->
  ?DEBUG("start_me_up/3"),
  loop(MM).

loop(MM) ->
  ?DEBUG("loop/1"),
  receive
    {chan, MM, {store, K, V}} ->
      ?DEBUG("loop/1 - {chan, _, {store, _, _}}"),
      kvs:store(K, V),
      loop(MM);
    {chan, MM, {lookup, K}} ->
      ?DEBUG("loop/1 - {chan, _, {lookup, _}}"),
      MM ! {send, kvs:lookup(K)},
      loop(MM);
    {chan_closed, MM} ->
      ?DEBUG("loop/1 - {chan_closed, _}"),
      true
  end.
