-module(area_server0).
-export([loop/0]).

loop() ->
  receive
    {rectangle, Width, Height} ->
      io:format("area of rectangle is ~p~n", [Width * Height]),
      loop();
    {circle, R} ->
      io:format("area of circle is ~p~n", [3.14 * R * R]),
      loop();
    Other ->
      io:format("I dont't know what the area of a ~p is ~n", [Other]),
      loop()
  end.
