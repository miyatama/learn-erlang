-module(indexer_checkpoint).

-export([init/2, resume/1, checkpoint/2]).
-import(filelib, [is_file/1]).

init(Dir, X) ->
    One = Dir ++ "/1.check",
    Two = Dir ++ "/2.check",
    case is_file(One) or is_file(Two) of
	true ->
	    exit(eBadInit);
	false ->
	    checkpoint({Dir, 1}, X),
	    checkpoint({Dir, 2}, X)
    end.

resume(Dir) ->
    R1 = recover(Dir ++ "/1.check"),
    R2 = recover(Dir ++ "/2.check"),
    case {R1, R2} of
	{error, error}               -> error;
	{error, _}                   -> {{Dir,1}, element(2, R2)};
	{_, error}                   -> {{Dir,2}, element(2, R1)};
	{{T1,X},{T2,_}} when T1 > T2 -> {{Dir,2}, X};
	{_,{_,X}}                    -> {{Dir,1}, X}
    end.

recover(File) ->
    case file:read_file(File) of
	{ok, Bin} when size(Bin) > 32 ->
	    {B1,B2} = split_binary(Bin, 16),
	    case bin_to_md5(B2) of
		B1 ->
		    binary_to_term(B2);
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

checkpoint({Dir, Next}, X) ->
    File = Dir ++ "/" ++ integer_to_list(Next) ++ ".check",
    Time = erlang:system_time(),
    B = term_to_binary({Time, X}),
    CheckSum = bin_to_md5(B),
    ok = file:write_file(File, [CheckSum,B]),
    {Dir, 3-Next}.
    
bin_to_md5(Bin) ->
    C1 = erlang:md5_init(),
    C2 = erlang:md5_update(C1, Bin),
    erlang:md5_final(C2).
