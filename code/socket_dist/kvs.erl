% c(kvs).
% kvs:start().
% kvs:store({location, joe}, "Stockholm").
% kvs:store(weather, raining).
% kvs:lookup({location, joe}).
% kvs:lookup(weather).
-module(kvs).
-export([start/0, store/2, lookup/1]).
-define(DEBUG(Str), io:fwrite("[DEBUG] kvs:" ++ Str ++ "~n")).

start() ->
  ?DEBUG("start/0"),
  register(kvs, spawn(fun() -> loop() end)).

store(Key, Value) ->
  ?DEBUG("store/2"),
  rpc({store, Key, Value}).

lookup(Key) ->
  ?DEBUG("lookup/1"),
  rpc({lookup, Key}).

rpc(Q) ->
  ?DEBUG("rpc/1"),
  kvs ! {self(), Q},
  receive
    {kvs, Replay} -> 
      ?DEBUG("rpc/1 - {kvs, _}"),
      Replay
  end.

loop() ->
  ?DEBUG("loop/0"),
  receive
    {From, {store, Key, Value}} ->
      ?DEBUG("loop/0 - {_, {store, _, _}}"),
      put(Key, {ok, Value}),
      From ! {kvs, true},
      loop();
    {From, {lookup, Key}} ->
      ?DEBUG("loop/0 - {_, {lookup, _}}"),
      From ! {kvs, get(Key)},
      loop()
  end.
