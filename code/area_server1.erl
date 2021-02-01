%c(area_server1).
-module(area_server1).
-export([loop/0, rpc/2]).

rpc(Pid, Request) -> 
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

% Pid = spawn(fun area_server1:loop/0).
% area_server1:rpc(Pid, {rectangle, 8, 2}).
% area_server1:rpc(Pid, {circle, 2}).
% area_server1:rpc(Pid, {sylinder, 8, 12, 9}).
loop() ->
  receive
    {From, {rectangle, Width, Height}} ->
      From ! {self(), {ok, Width * Height}},
      loop();
    {From, {circle, R}} ->
      From ! {self(), {ok, 3.14 * R * R}},
      loop();
    {From, Other} ->
      From ! {self(), {error, Other}},
      loop()
  end.
