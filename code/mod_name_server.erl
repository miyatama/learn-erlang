-module(mod_name_server).
-export([start_me_up/3]).

start_me_up(MM, _ArgC, _ArgS) ->
  loop(MM).

loop(MM) ->
  receive
    {chan, MM, {store, K, V}} ->
      kvs:store(K, V),
      loop(MM);
    {chan, MM, {lookup, K}} ->
      kvs:lookup(K),
      loop(MM);
    {chan_closed, MM} ->
      true
  end.
