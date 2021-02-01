-module(area_server_final).
-export([start/0, area/2]).

% Pid = area_server_final:start().
start() ->
  spawn(fun loop/0).

% area_server_final:area(Pid, {rectangle, 15, 2}).
% area_server_final:area(Pid, {circle, 20}).
% area_server_final:area(Pid, {circle, error}).
area(Pid, What) ->
  rpc(Pid, What).

rpc(Pid, Request) -> 
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

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
