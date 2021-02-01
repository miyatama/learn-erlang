-module(clock).
-export([start/2, stop/0]).

% c(clock).
% clock:start(1000, fun() -> io:format("tick ~p~n", [erlang:now()]) end).
% clock:stop().
start(Time, Fun) ->
  register(clock, spawn(fun() -> tick(Time, Fun) end)).

stop() ->
  clock ! stop.

tick(Time, Fun) -> 
  receive
    stop -> void
  after Time ->
    Fun(),
    tick(Time, Fun)
  end.
