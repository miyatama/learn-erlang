-module(lib_lin).

-export([pow/3,
	inv/2,
	solve/2,
	str2int/1,
	int2str/1,
	gcd/2]).

% -define(DEBUG(S), io:fwrite("[DEBUG] lib_lin: " ++ S ++ "~n")).
-define(DEBUG(S), io:fwrite("")).

pow(A, 1, M) ->
	?DEBUG("pow/3"),
	A rem M;
pow(A, 2, M) ->
	?DEBUG("pow/3"),
	A*A rem M;
pow(A, B, M) ->
	?DEBUG("pow/3"),
	B1 = B div 2,
	B2 = B - B1,
	P = pow(A, B1, M),
	case B2 of
		B1 -> (P*P) rem M;
		_ -> (P*P*A) rem M
	end.

inv(A, B) ->
	?DEBUG("inv/2"),
	case solve(A, B) of
		{X, _} ->
			if 
				X < 0 -> X + B;
				true -> X
			end;
		_ ->
			no_inverse
	end.

solve(A, B) ->
	?DEBUG("solve/2"),
	case catch s(A, B) of
		insoluble -> insoluble;
		{X, Y} ->
			case A * X - B * Y of
				1 -> {X, Y};
				_Other -> error
			end
	end.

s(_, 0) -> throw(insoluble);
s(_, 1) -> {0, -1};
s(_, -1) -> {0, 1};
s(A, B) ->
	K1 = A div B,
	K2 = A - K1*B,
	{Tmp, X} = s(B, -K2),
	{X, K1 * X - Tmp}.

str2int(Str) ->
	str2int(Str, 0).

str2int([H|T], N) ->
	str2int(T, N*256+H);
str2int([], N) -> 
	N.

int2str(N) -> 
	int2str(N, []).

int2str(N, L) when N =< 0 -> 
	L;
int2str(N, L) ->
	N1 = N div 256,
	H = N - N1 * 256,
	int2str(N1, [H|L]).

gcd(A, B) when A < B -> gcd(B, A);
gcd(A, 0)  -> A;
gcd(A, B) -> gcd(B, A rem B).
	    
