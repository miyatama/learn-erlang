-module(stimer).
-export([start/2, cancel/1]).

% c(stimer).
% Pid = stimer:start(5000, fun() -> io:format("timer event~n") end).
% wait a 5[sec]
% stimer:cancel(Pid).
start(Time, Fun) ->
  spawn(fun() -> timer(Time, Fun) end).

cancel(Pid) ->
  Pid ! cancel.

timer(Time, Fun) ->
  receive
    cancel ->
      void
  after Time ->
    Fun()
  end.
