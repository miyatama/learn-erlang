-module(indexer_dir_crawler).

-export([start/1, next/1, test/0]).

-import(lists, [foldl/3, suffix/2, sublist/3, map/2, filter/2, reverse/1]).
-import(indexer_lib, [is_type/3]).

test() ->
    loop(next(start(["/home/ejoearm/erl/progs/doc/docbuilder"]))).

loop({dir, Dir, Cont}) ->
    io:format("Dir=~p~n",[Dir]),
    loop(next(Cont));
loop(done) ->
    io:format("done:~n").

start(L) ->
    L.

next([]) ->
    done;
next([{Dir,[H1|T1]}|T2]) ->
    FullDirName = Dir++"/" ++ H1,
    next([FullDirName,{Dir,T1}|T2]);
next([{_,[]}|T]) ->
    next(T);
next([Dir|T]) ->
    case file:list_dir(Dir) of
	{ok, Things} -> 
	    Dirs = filter(fun(I) -> is_dir(I, Dir) end, Things),
	    More = case Dirs of
		       [] -> T;
		       _  -> [{Dir,Dirs}|T]
		   end,
	    {dir, Dir,More};
	{error, _}  -> 
	    next(T)
    end.

is_dir(I, Dir) ->
    filelib:is_dir(filename:join(Dir, I)).
