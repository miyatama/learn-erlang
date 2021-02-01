-module(prime_server).

-behavior(gen_server).

-export([new_prime/1, start_link/0]).
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(DEBUG(S), io:fwrite("[DEBUG] prime_server: " ++ S ++ "~n")).

start_link() ->
	?DEBUG("start_link/0"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_prime(N) ->
	?DEBUG("new_prime/1"),
	% timeout: 5000[ms]
	gen_server:call(?MODULE, {prime, N}, 5000).

init([]) ->
	?DEBUG("init/1"),
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

handle_call({prime, K}, _From, N) ->
	?DEBUG("handle_call/3"),
	{reply, make_new_prime(K), N+1}.

handle_cast(_Msg, N) ->
	?DEBUG("handle_cast/2"),
	{noreply, N}.

handle_info(_Info, N) ->
	?DEBUG("handle_info/2"),
	{noreply, N}.

terminate(_Reason, _N) ->
	?DEBUG("terminate/2"),
	io:format("~p stopping~n", [?MODULE]),
	ok.
	
code_change(_OldVsn, N, _Extra) ->
	?DEBUG("code_change/3"),
	{ok, N}.

make_new_prime(K) ->
	?DEBUG("make_new_prime/1"),
	if
		K > 100 ->
			?DEBUG("make_new_prime/1 - K over 100"),
			alarm_handler:set_alarm(too_hot),
			lib_primes:make_prime(K),
			alarm_handler:clear_alarm(too_hot);
		true ->
			?DEBUG("make_new_prime/1 - K not over 100"),
			lib_primes:make_prime(K)
	end.
