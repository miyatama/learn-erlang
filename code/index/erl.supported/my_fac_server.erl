-module(my_fac_server).
-export([loop/0]).
-define(DEBUG(S), io:fwrite("[DEBUG] my_fac_server: " ++ S ++ "~n")).

loop() ->
	?DEBUG("loop/0"),
	receive
		{From, {fac, N}} ->
			From ! {self(), fac(N)},
			loop();
		{become, Something} ->
			Something()
	end.

fac(0) -> 1;
fac(N) -> N * fac(N - 1).
