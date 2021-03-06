-module(lib_primes).

-export([make_prime/1,
	is_prime/1,
	make_random_int/1]).

% -define(DEBUG(S), io:fwrite("[DEBUG] lib_primes: " ++ S ++ "~n")).
-define(DEBUG(S), io:fwrite("")).

make_prime(1) ->
	?DEBUG("make_prime/1"),
	lists:nth(rand:uniform(4), [2, 3, 5, 7]);
make_prime(K) ->
	?DEBUG("make_prime/1"),
	new_seed(),
	N = make_random_int(K),
	if N > 3 ->
		   io:format("generate a ~w digit prime ", [K]),
		   MaxTries = N - 3,
		   P1 = make_prime(MaxTries, N+1),
		   io:format("~nprime is ~w~n", [P1]),
		   P1;
	   true->
		   make_prime(K)
	end.

make_prime(0, _) ->
	?DEBUG("make_prime/2"),
	exit(impossible);
make_prime(K, P) ->
	?DEBUG("make_prime/2"),
	io:format(".", []),
	case is_prime(P) of
		true -> P;
		false -> make_prime(K-1, P+1)
	end.

is_prime(D) when D < 10 ->
	?DEBUG("is_prime/1"),
	lists:member(D, [2,3,5,7]);
is_prime(D) ->
	?DEBUG("is_prime/1"),
	new_seed(),
	is_prime(D, 100).

is_prime(D, Ntests) ->
	?DEBUG("is_prime/2"),
	N = length(integer_to_list(D)) -1,
	is_prime(Ntests, D, N).

is_prime(0, _, _) ->
	?DEBUG("is_prime/3"),
	true;
is_prime(Ntest, N, Len) ->
	?DEBUG("is_prime/3"),
	K = rand:uniform(Len),
	A = make_random_int(K),
	if
		A < N ->
			case lib_lin:pow(A, N, N) of
				A -> is_prime(Ntest -1, N, Len);
				_ -> false
			end;
		true ->
			is_prime(Ntest, N, Len)
	end.

make_random_int(N) ->
	?DEBUG("make_random_int/1"),
	new_seed(),
	make_random_int(N, 0).

make_random_int(0, D) ->
	?DEBUG("make_random_int/2"),
	D;
make_random_int(N, D) ->
	?DEBUG("make_random_int/2"),
	make_random_int(N-1, D*10 + (rand:uniform(10) -1)).

new_seed() ->
	X = erlang:monotonic_time(),
	{H, M, S} = time(),
	H1 = H * X rem 32767,
	M1 = M * X rem 32767,
	S1 = S * X rem 32767,
	put(random_seed, {H1, M1, S1}).




