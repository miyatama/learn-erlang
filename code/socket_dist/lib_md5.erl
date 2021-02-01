-module(lib_md5).
-export([string/1,
	 file/1,
	 bin/1,
	 binAsBin/1,
	 digest2str/1]).

-define(BLOCKSIZE, 32768).
-define(IN(X, Min, Max), X >= Min, X =< Max).

string(Str) ->
	digest2str(erlang:md5(Str)).

file(File) ->
	case file:open(File) of
		{ok, P} ->
			loop(P, erlang:md5_init());
		Error -> 
			Error
	end.

bin(Bin) ->
	C1 = erlang:md5_init(),
	C2 = erlang:md5_update(C1, Bin),
	C3 = erlang:md5_final(C2),
	digest2str(C3).

binAsBin(Bin) ->
	C1 = erlang:md5_init(),
	C2 = erlang:md5_update(C1, Bin),
	erlang:md5_final(C2).

digest2str(Digest) ->
	bin2str(binary_to_list(Digest)).

loop(P, C) ->
	case file:read(P, ?BLOCKSIZE) of
		{ok, Bin} ->
			loop(P, erlang:md5_update(C, Bin));
		eof ->
			file:close(P),
			{ok, erlang:md5_final(C)}
	end.

bin2str([H|T]) ->
	{H1, H2} = byte2hex(H),
	[H1,H2|bin2str(T)];
bin2str([]) -> [].

byte2hex(X) ->
	{nibble2hex(X bsr 4), nibble2hex(X band 15)}.

nibble2hex(X) when ?IN(X, 0, 9) -> 
	X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> 
	X - 10 + $a.

