-module(shout).
-export([start/0]).
-import(lists, [map/2, reverse/1]).

-define(CHUNKSIZE, 24576).
-define(DEBUG(S), io:fwrite("[DEBUG] shout: " ++ S ++ "~n")).

start() ->
	?DEBUG("start/0"),
	spawn(fun() ->
			start_parallel_server(3000),
			lib_misc:sleep(infinity)
		end).

start_parallel_server(Port) ->
	?DEBUG("start_parallel_server/1"),
	{ok, Listen} = gen_tcp:listen(Port, 
				      [binary,
				       {packet, 0},
				       {active, true},
				       {reuseaddr, true}]),
	PidSongServer = spawn(fun() -> songs() end),
	spawn(fun() -> par_connect(Listen, PidSongServer) end).

par_connect(Listen, PidSongServer) ->
	?DEBUG("par_connect/2"),
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> par_connect(Listen, PidSongServer) end),
	inet:setopts(Socket,
		     [{packet, 0},
		      binary,
		      {nodelay, true},
		      {active, true}]),
	get_request(Socket, PidSongServer, []).

get_request(Socket, PidSongServer, L) ->
	?DEBUG("get_request/3"),
	receive
		{tcp, Socket, Bin} ->
			L1 = L ++ binary_to_list(Bin),
			case split(L1, []) of
				more ->
					get_request(Socket, PidSongServer, L1);
				{Request, _Rest} ->
					got_request_from_client(Request, Socket, PidSongServer)
			end;
		{tcp_closed, Socket} ->
			void;
		_Any ->
			get_request(Socket, PidSongServer, L)
	end.

split("\r\n\r\n" ++ T, L) -> 
	?DEBUG("split/2"),
	{reverse(L), T};
split([H|T], L) -> 
	?DEBUG("split/2"),
	split(T, [H|L]);
split([], _) -> 
	?DEBUG("split/2"),
	more.

got_request_from_client(Request, Socket, PidSongServer) ->
	?DEBUG("got_request_from_client/3"),
	Cmds = string:tokens(Request, "\r\n"),
	Cmds1 = map(fun(I) -> string:tokens(I, " ") end, Cmds),
	is_request_for_stream(Cmds1),
	gen_tcp:send(Socket, [response()]),
	play_songs(Socket, PidSongServer, <<>>).

play_songs(Socket, PidSongServer, SoFar) ->
	?DEBUG("play_songs/3"),
	Song = rpc(PidSongServer, random_song),
	{File, PrintStr, Header} = unpack_song_descriptor(Song),
	case id3_tag_lengths:file(File) of
		error ->
			play_songs(Socket, PidSongServer, SoFar);
		{Start, Stop} ->
			io:formt("playing: ~p~n", [PrintStr]),
			{ok, S} = file:open(File, [read, binary, raw]),
			SoFar1 = send_file(S, {0, Header}, Start, Stop, Socket, SoFar),
			file:close(S),
			play_songs(Socket, PidSongServer, SoFar1)
	end.

send_file(S, Header, Offset, Stop, Socket, SoFar) ->
	?DEBUG("send_file/6"),
	Need = ?CHUNKSIZE - size(SoFar),
	Last = Offset + Need,
	if
		Last >= Stop ->
			Max = Stop - Offset,
			Bin = file:pread(S, Offset, Max),
			list_to_binary([SoFar, Bin]);
		true ->
			{ok, Bin} = file:pread(S, Offset, Need),
			write_data(Socket, SoFar, Bin, Header),
			send_file(S, bump(Header), Offset + Need, Stop, Socket, <<>>)
	end.

write_data(Socket, B0, B1, Header) ->
	?DEBUG("write_data/4"),
	case size(B0) + size(B1) of
		?CHUNKSIZE ->
			case gen_tcp:send(Socket, [B0, B1, the_header(Header)]) of
				ok -> true;
				{error, closed} ->
					exit(playerClosed)
			end;
		_Other ->
			io:format("block length error: B0 = ~p, B1 = ~p~n", [size(B0), size(B1)])
	end.

bump({K, H}) -> 
	?DEBUG("bump/1"),
	{K + 1, H}.

the_header({K, H}) ->
	?DEBUG("the_header/1"),
	case K rem 5 of
		0 -> H;
		_ -> <<0>>
	end.

is_request_for_stream(_) -> 
	?DEBUG("is_request_for_stream/1"),
	true.

response() ->
	?DEBUG("response/0"),
	["ICY 200 OK\r\n"
	"icy-notice1: <BR>This stream requires"
	"<a href=\"http://www.winamp.com/\">Winamp</a><BR>\r\n"
	"icy-notice2: erlang shoutcast sever<br>\r\n"
	"icy-name: erlan mix\r\n"
	"icy-genre: pop top 40 dance rock\r\n"
	"icy-url: http://localhost:3000\r\n"
	"content-type: audio/mpeg\r\n"
	"icy-pub: 1\r\n"
	"icy-metaint: ", integer_to_list(?CHUNKSIZE) , "\r\n"
	"icy-br: 96\r\n\r\n"].

songs() ->
	?DEBUG("songs/0"),
	{ok, [SongList]} = file:consult("mp3data.tmp"),
	lib_misc:random_seed(),
	songs_loop(SongList).

songs_loop(SongList) ->
	?DEBUG("songs_loop/1"),
	receive
		{From, random_song} ->
			I = rand:uniform(length(SongList)),
			Song = lists:nth(I, SongList),
			From ! {self(), Song},
			songs_loop(SongList)
	end.

rpc(Pid, Q) ->
	?DEBUG("rpc/2"),
	Pid ! {self(), Q},
	receive
		{Pid, Reply} ->
			Reply
	end.

unpack_song_descriptor({File, {_Tag, Info}}) ->
	?DEBUG("unpack_song_descriptor/1"),
	PrintStr = list_to_binary(make_header1(Info)),
	L1 = ["StreamTitle='", PrintStr, "';StreamUrl='http://localhost:3000';"],
	Bin = list_to_binary(L1),
	Nblocks = ((size(Bin) - 1) div 16) + 1,
	NPad = Nblocks*16 - size(Bin),
	Extra = lists:duplicate(NPad, 0),
	Header = list_to_binary([Nblocks, Bin, Extra]),
	{File, PrintStr, Header}.

make_header1([{track, _} | T]) ->
	?DEBUG("make_header1/1"),
	make_header1(T);
make_header1([{Tag, X} | T]) ->
	?DEBUG("make_header1/1"),
	[atom_to_list(Tag), ": ", X, " " | make_header1(T)];
make_header1([]) -> 
	?DEBUG("make_header1/1"),
	[].
