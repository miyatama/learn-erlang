-module(mp3_v1).
-import(lists, [filter/2, map/2, reverse/1]).
-export([test/0,
	 dir/1,
	 read_id3_tag/1]).
-define(DEBUG(S), io:fwrite("[DEBUG] mp3_v1: " ++ S ++ "~n")).

test() ->
	?DEBUG("test()"),
	dir("./").

dir(Dir) ->
	?DEBUG("dir()"),
	Files = lib_find:files(Dir, "^.*\.mp3$", true),
	L1 = map(fun(I) ->
			{I, (catch read_id3_tag(I))}
		end, Files),
	L2 = filter(fun({_, error}) ->false;
		       (_) -> true
		end, L1),
	lib_misc:dump("mp3data", L2).

read_id3_tag(File) ->
	?DEBUG("read_id3_tag() - File: " ++ File),
	case file:open(File, [read, binary, raw]) of
		{ok, S} ->
			Size = filelib:file_size(File),
			{ok, B2} = file:pread(S, Size - 128, 128),
			Result = parse_v1_tag(B2),
			file:close(S),
			Result;
		Error ->
			{File, Error}
	end.

parse_v1_tag(<<$T, $A, $G,
	       Title:30/binary,
	       Artist:30/binary,
	       Album:30/binary,
	       Year:4/binary,
	       _Comment:28/binary,
	       0:8,
	       Track:8,
	       _Genre:8>>) ->
	?DEBUG("parse_v1_tag() - v1.1"),
	{"ID3v1.1",
	 [{track, Track},
	  {title, trim(Title)},
	  {artist, trim(Artist)},
	  {album, trim(Album)},
	  {year, Year}]};
parse_v1_tag(<<$T, $A, $G,
	       Title:30/binary,
	       Artist:30/binary,
	       Album:30/binary,
	       Year:4/binary,
	       _Comment:30/binary,
	       _Genre:8>>) ->
	?DEBUG("parse_v1_tag() - v1"),
	{"ID3v1",
	 [{title, trim(Title)},
	  {artist, trim(Artist)},
	  {album, trim(Album)},
	  {year, Year}]};
parse_v1_tag(_) -> 
	?DEBUG("parse_v1_tag() - error"),
	error.

trim(Bin) ->
	list_to_binary(trim_branks(binary_to_list(Bin))).

trim_branks(X) ->
	reverse(skip_blanks_and_zero(reverse(X))).
skip_blanks_and_zero([$\s|T]) ->
	skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T]) ->
	skip_blanks_and_zero(T);
skip_blanks_and_zero(X) -> X.

