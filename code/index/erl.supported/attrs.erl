-module(attrs).
-vsn(1).
-author({miyata}).
-purpose("example of attributes").
-export([fac/1]).

fac(1) -> 1;
fac(N) -> N * fac(N -1).
