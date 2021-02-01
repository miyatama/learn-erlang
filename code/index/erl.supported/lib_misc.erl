-module(lib_misc).

-export([consult/1,
	 dump/2, 
	 first/1, 
	 for/3,
	 is_prefix/2,
	 deliberate_error/1,
	 deliberate_error1/1,
	 duplicates/1,
	 downcase_char/1,
	 downcase_str/1,
	 extract_attribute/2,
	 eval_file/1,
	 every/3,
	 file_size_and_type/1,
	 flush_buffer/0,
	 foreachWordInFile/2,
	 foreachWordInString/2,
	 keep_alive/2,
	 glurk/2,
	 lookup/2,
	 odds_and_evens/1,
	 odds_and_evens_acc/1,
	 on_exit/2,
	 make_global/2,
	 make_test_strings/1,
	 merge_kv/1,
	 ndots/1,
	 test_function_over_substrings/2,
	 partition/2,
	 pmap/2,
	 pmap1/2,
	 priority_receive/0,
	 pythag/1, 
	 replace/3,
	 split/2,
	 safe/1,
	 too_hot/0,
	 ls/1,
	 mini_shell/0,
	 odd/1,
	 outOfDate/2,
	 exists/1,
	 perms/1,
	 qsort/1,
	 random_seed/0,
	 read_file_as_lines/1,
	 remove_duplicates/1,
	 remove_prefix/2,
	 remove_leading_whitespace/1,
	 remove_trailing_whitespace/1,
	 rpc/2,
	 spawn_monitor/3,
	 sum/1,
	 sqrt/1,
	 string2term/1,
	 string2value/1,
	 term2string/1,
	 term2file/2,
	 file2term/1,
	 longest_common_prefix/1,
	 unconsult/2]).

-export([complete/2, 
	 skip_blanks/1, trim_blanks/1, sleep/1, split_at_char/2,
	 %% mk_tree/1, 
	 is_blank_line/1, 
	 have_common_prefix/1]).

-import(lists, [all/2, any/2, filter/2, reverse/1, reverse/2,
		foreach/2, map/2, member/2, sort/1]).


-define(NYI(X),(begin 
			io:format("*** NYI ~p ~p ~p~n",[?MODULE, ?LINE, X]),
			exit(nyi) 
		end)).

-define(DEBUG(S), io:fwrite("[DEBUG] lib_misc: " ++ S ++ "~n")).


glurk(X, Y) ->
	?NYI({glurk, X, Y}).

-include_lib("kernel/include/file.hrl").
file_size_and_type(File) ->
	case file:read_file_info(File) of
	{ok, Facts} ->
		{Facts#file_info.type, Facts#file_info.size};
	_ ->
		error
	end.
%% END:filesizeandtype	

%% START:ls
ls(Dir) ->
	{ok, L} = file:list_dir(Dir),
	map(fun(I) -> {I, file_size_and_type(I)} end, sort(L)).

%% START:consult
consult(File) ->
	case file:open(File, read) of
	{ok, S} ->
		Val = consult1(S),
		file:close(S),
		{ok, Val};
	{error, Why} ->
		{error, Why}
	end.

consult1(S) ->
	case io:read(S, '') of
	{ok, Term} -> [Term|consult1(S)];
	eof		-> [];
	Error	  -> Error
	end.


%% START:dump
dump(File, Term) ->
	?DEBUG("dump/2"),
	Out = File ++ ".tmp",
	io:format("** dumping to ~s~n",[Out]),
	{ok, S} = file:open(Out, [write]),
	io:format(S, "~p.~n",[Term]), 
	file:close(S).
%% END:dump

partition(F, L) -> partition(F, L, [], []).

partition(F, [H|T], Yes, No) ->
	case F(H) of
	true  -> partition(F, T, [H|Yes], No);
	false -> partition(F, T, Yes, [H|No])
	end;
partition(_, [], Yes, No) ->
	{Yes, No}.

remove_duplicates(L) ->
	remove_duplicates(lists:sort(L), []).

remove_duplicates([H|X=[H|_]], L) -> remove_duplicates(X, L);
remove_duplicates([H|T], L)	   -> remove_duplicates(T, [H|L]);
remove_duplicates([], L)		  -> L.

%% is_prefix(A, B) -> bool()
%%	AがBの先頭部分ならばtrue

is_prefix([], _)		 -> true;
is_prefix([H|T], [H|T1]) -> is_prefix(T, T1);
is_prefix(_, _)		  -> false.

first([_])   -> [];
first([H|T]) -> [H|first(T)].


sleep(T) ->
	receive
	after T ->
	   true
	end.



flush_buffer() ->
	receive
	_Any ->
		flush_buffer()
	after 0 ->
	true
	end.



priority_receive() ->
	receive
	{alarm, X} ->
		{alarm, X}
	after 0 ->
	receive
		Any ->
		Any
	end
	end.


duplicates(X) ->  find_duplicates(sort(X), []).

find_duplicates([H,H|T], [H|_]=L) ->
	find_duplicates(T, L);
find_duplicates([H,H|T], L) ->
	find_duplicates(T, [H|L]);
find_duplicates([_|T], L) ->
	find_duplicates(T, L);
find_duplicates([], L) ->
	L.


%% complete(A, L) -> {yes, S}
%%   error	 - 文字列がまったく一致しないことを示す
%%   {more,L}  - 補完候補はあるが、さらに文字が必要であることを示す
%%			   L = [Str] = 補完候補のリスト
%%   {yes, S}  - 補完候補が1つしかないことを示す
%%   
%%   A = S = str(), L=[str()]
%%   A ++ S がLのすべての要素のメンバであるような
%%   最小のSを計算するために使われる

complete(Str, L) ->
	case filter(fun(I) -> is_prefix(Str, I) end, L) of
	[] ->
		error;
	[L1] ->
		J = remove_prefix(Str, L1),
		{yes, J};
	L1 ->
		%% L1は空ではないので、さらに候補があるか、補完文字列が1つだけ存在する
		%% StrがL1の全要素の先頭部分であることはわかっている
		L2 = map(fun(I) -> remove_prefix(Str, I) end, L1),
		%% L2も空ではないはず
		%% io:format("L1=~p L2=~p~n",[L1,L2]),
		case longest_common_prefix(L2) of
		[] ->
			{more, L1};
		S ->
			{yes, S}
		end
	end.

%% remove_prefix(X, Y) -> Z
%%   X ++ Z = Y を満たすようなZを見つける
%%   
remove_prefix([H|T], [H|T1]) -> remove_prefix(T, T1);
remove_prefix([], L)		 -> L.

%% longest_common_prefix([str()]) -> str()

longest_common_prefix(L) ->
	longest_common_prefix(L, []).

longest_common_prefix(Ls, L) ->
	case have_common_prefix(Ls) of
	{yes, H, Ls1} ->
		longest_common_prefix(Ls1, [H|L]);
	no ->
		reverse(L)
	end.

have_common_prefix([]) -> no;
have_common_prefix(L) ->
	case any(fun is_empty_list/1, L) of
	true  -> no;
	false ->
		%% どのリストにもヘッドとテールがある
		Heads = map(fun(I) -> hd(I) end, L),
		H = hd(Heads),
		case all(fun(X) -> hd(X) =:= H end, L) of
		true -> 
			Tails = map(fun(I) -> tl(I) end, L),
			{yes, H, Tails};
		false ->
			no
		end
	end.

is_empty_list([]) ->	true;
is_empty_list(X) when is_list(X) -> false.

skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks(X)	   -> X.
	
trim_blanks(X) -> reverse(skip_blanks(reverse(X))).
	

split_at_char(Str, C) -> split_at_char(Str, C, []).

split_at_char([C|T], C, L) -> {yes, reverse(L), T};
split_at_char([H|T], C, L) -> split_at_char(T, C, [H|L]);
split_at_char([], _, _)	-> no.

%% ファイルを行バッファに読み込む
read_file_as_lines(File) ->	
	case file:read_file(File) of
	{ok, Bin} ->
		{ok, split_into_lines(binary_to_list(Bin), 1,  [])};
	{error, _} ->
		{error, eNoFile}
	end.

split_into_lines([], _, L) ->
	reverse(L);
split_into_lines(Str, Ln, L) ->
	{Line, Rest} = get_line(Str, []),
	split_into_lines(Rest, Ln+1, [{Ln,Line}|L]).

get_line([$\n|T], L) -> {reverse(L), T};
get_line([H|T], L)   -> get_line(T, [H|L]);
get_line([], L)	  -> {reverse(L), []}.

is_blank_line([$\s|T]) -> is_blank_line(T);
is_blank_line([$\n|T]) -> is_blank_line(T);
is_blank_line([$\r|T]) -> is_blank_line(T);
is_blank_line([$\t|T]) -> is_blank_line(T);
is_blank_line([]) -> true;
is_blank_line(_)  -> false.

%%----------------------------------------------------------------------
%% lookup
		   
%%----------------------------------------------------------------------
%% split(Pred, L) -> {True, False}

split(F, L) -> split(F, L, [], []).

split(F, [H|T], True, False) ->
	case F(H) of
	true  -> split(F, T, [H|True], False);
	false -> split(F, T, True, [H|False])
	end;
split(_, [], True, False) ->
	{reverse(True), reverse(False)}.
 
%%----------------------------------------------------------------------

outOfDate(In, Out) ->
	case exists(Out) of
	true ->
		case {last_modified(In), last_modified(Out)} of
		{T1, T2} when T1 > T2 ->
			true;
		_ ->
			false
		end;
	false ->
		true
	end.

last_modified(File) ->
	case file:read_file_info(File) of
		{ok, Info} ->
			Info#file_info.mtime;
		_ ->
			0
	end.

exists(File) ->
	case file:read_file_info(File) of
		{ok, _} ->
			true;
		_ ->
			false
	end.

%%----------------------------------------------------------------------
%% replace(Key,Val, [{Key,Val}]) -> [{Key,Val}]
%%  連想リストOldのKeyをKey, Valで置き換える

replace(Key, Val, Old) ->
	replace(Key, Val, Old, []).

replace(Key, Val1, [{Key,_Val}|T], L) ->
	reverse(L, [{Key, Val1}|T]);
replace(Key, Val, [H|T], L) ->
	replace(Key, Val, T, [H|L]);
replace(Key, Val, [], L) ->
	[{Key,Val}|L].

%%----------------------------------------------------------------------
%% make_test_strings(Str)
%%

make_test_strings(Str) ->
	L = length(Str),
	make_test_strings(Str, L+1, 1).

make_test_strings(_, Max, Max) -> [];
make_test_strings(Str, Max, N) ->
	[string:sub_string(Str, 1, N)|make_test_strings(Str, Max, N+1)].

test_function_over_substrings(F, Str) ->
	L = make_test_strings(Str),
	foreach(fun(S) ->
			io:format("|~s|~n	=> ~p~n", [S, F(S)])
		end, L).

%%----------------------------------------------------------------------
%% merge_kv(Kv) -> Kv'
%%	{Key, Val} (Keyは複数個あってもよい)の連想リストを受け取って、
%%	Keyがそれぞれ1つしかないようなリスト{Key, [Val]}に変換する

merge_kv(KV) ->  merge_kv(KV, dict:new()).

merge_kv([{Key,Val}|T], D0) ->
	case dict:find(Key, D0) of
	{ok, L} -> merge_kv(T, dict:store(Key, [Val|L], D0));
	error   -> merge_kv(T, dict:store(Key, [Val], D0))
	end;
merge_kv([], D) ->
	dict:to_list(D).


%% rpc/2
%% 
rpc(Pid, Q) ->
	Pid ! {self(), Q},
	receive
	{Pid, Reply} ->
		Reply
	end.

%% odd(X)
%%

odd(X) ->
	case X band 1 of
	1 -> true;
	0 -> false
	end.
		
ndots([$.|T]) -> 1 + ndots(T);
ndots([_|T])  -> ndots(T);
ndots([])	 -> 0.

%% START:term2file
term2file(File, Term) ->
	file:write_file(File, term_to_binary(Term)).

file2term(File) ->
	{ok, Bin} = file:read_file(File),
	binary_to_term(Bin).
%% END:term2file

%% START:string2term
string2term(Str) ->
	{ok,Tokens,_} = erl_scan:string(Str ++ "."),
	{ok,Term} = erl_parse:parse_term(Tokens),
	Term.

term2string(Term) ->
	lists:flatten(io_lib:format("~p",[Term])).
%% END:string2term

%% START:downcasestr
downcase_str(Str) -> map(fun downcase_char/1, Str).

downcase_char(X) when $A =< X, X =< $Z -> X+ $a - $A;
downcase_char(X)					   -> X.
%% END:downcasestr

%% START:string2value
string2value(Str) ->
	{ok, Tokens, _} = erl_scan:string(Str ++ "."),
	{ok, Exprs} = erl_parse:parse_exprs(Tokens),
	Bindings = erl_eval:new_bindings(),
	{value, Value, _} = erl_eval:exprs(Exprs, Bindings),
	Value.
%% END:string2value

%% START:mini_shell
mini_shell() ->
	mini_shell(erl_eval:new_bindings()).

mini_shell(Bindings0) ->
	case io:get_line('>>> ') of
	"q\n" -> void;
	Str ->
		{Value, Bindings1} = string2value(Str, Bindings0),
		io:format("~p~n",[Value]),
		mini_shell(Bindings1)
	end.

string2value(Str, Bindings0) ->
	{ok, Tokens, _} = erl_scan:string(Str ++ "."),
	{ok, Exprs} = erl_parse:parse_exprs(Tokens),
	{value, Value, Bindings1} = erl_eval:exprs(Exprs, Bindings0),
	{Value, Bindings1}.
%% END:mini_shell
		
%% START:eval_file
eval_file(File) ->
	{ok, S} = file:open(File, [read]),
	Vals = eval_file(S, 1, erl_eval:new_bindings()),
	file:close(S),
	Vals.

eval_file(S, Line, B0) ->
	case io:parse_erl_exprs(S, '', Line) of
	{ok, Form, Line1} ->
		{value, Value, B1} = erl_eval:exprs(Form, B0),
		[Value|eval_file(S, Line1, B1)];
	{eof, _} ->
		[]
	end.
%% END:eval_file

%% START:remove_leading_whitespace
remove_leading_whitespace([$\n|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\s|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\t|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace(X) -> X.
%% END: remove_leading_whitespace

%% START:remove_trailing_whitespace
remove_trailing_whitespace(X) ->
	reverse(remove_leading_whitespace(reverse(X))).
%% END:remove_trailing_whitespace


safe(Fun) ->
	case (catch Fun()) of
	{'EXIT', Why} ->
		{error, Why};
	Other ->
		Other
	end.



too_hot() ->
	event_handler:event(errors, too_hot).


%% spawn_monitorはspawnのように機能する


spawn_monitor(_, false, Fun) ->
	spawn(Fun);
spawn_monitor(Term, true, Fun) ->
	spawn(fun() -> starter(Term, Fun) end).

starter(Term, Fun) ->
	S = self(),
	io:format("process:~p started at:~p ~p~n",
		  [self(), erlang:system_time(), Term]),
	Monitor = spawn_link(fun() -> monitor2(Term, S) end),
	receive
	{Monitor, ready} ->
		Fun()
	end.

monitor2(Term, Parent) ->
	process_flag(trap_exit, true),
	Parent ! {self(), ready},
	receive
	{'EXIT', Parent, Why} ->
		io:format("process:~p dies at:~p ~p reason:~p~n",
			  [self(), erlang:system_time(), Term, Why])
	end.


keep_alive(Name, Fun) ->
	register(Name, Pid = spawn(Fun)),
	on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).


%% make_global(Name, Fun)は登録名Nameを持つグローバルプロセスが存在
%% するか調べる。そのようなプロセスがなければ、Fun()を評価するプロセスを
%% 生成して、名前をNameとして登録する



make_global(Name, Fun) ->
	S = self(),
	Pid = spawn(fun() -> make_global(S, Name, Fun) end),
	receive
	{Pid, Reply} ->
		Reply
	end.
		
make_global(Parent, Name, Fun) ->	
	case (catch register(Name, self())) of
	true -> Fun();
	_ -> true
	end,
	Parent ! {self(), ok}.


%% on_exit(Pid, Fun)はPidにリンクする。Pidが要因Whyで死んだら
%% Fun(Why)を評価する：


on_exit(Pid, Fun) ->
	spawn(fun() -> 
		  process_flag(trap_exit, true), %% <label id="code.onexit1"/>
		  link(Pid),					 %% <label id="code.onexit2"/>
		  receive
			  {'EXIT', Pid, Why} ->	  %% <label id="code.onexit3"/>
			  Fun(Why)   %% <label id="code.onexit4"/>
		  end
	  end).


%% every(Pid, Time, Fun)はPidにリンクし、TimeごとにFun()を
%% 評価する。Pidが終了すると、このプロセスは停止する。


every(Pid, Time, Fun) ->
	spawn(fun() ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  every_loop(Pid, Time, Fun)
	  end).

every_loop(Pid, Time, Fun) ->
	receive
	{'EXIT', Pid, _Why} ->
		true
	after Time ->
		Fun(),
		every_loop(Pid, Time, Fun)
	end.



for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I)|for(I+1, Max, F)].



qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, X < Pivot])
	++ [Pivot] ++
	qsort([X || X <- T, X >= Pivot]).



perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].



pythag(N) ->
	[ {A,B,C} ||
		A <- lists:seq(1,N),
		B <- lists:seq(1,N),
		C <- lists:seq(1,N),
		A+B+C =< N,
		A*A+B*B =:= C*C 
	].



extract_attribute(File, Key) ->
	case beam_lib:chunks(File,[attributes]) of
	{ok, {attrs, [{attributes,L}]}} ->
		lookup(Key, L);
	_ -> exit(badFile)
	end.

lookup(Key, [{Key,Val}|_]) -> {ok, Val};
lookup(Key, [_|T])		 -> lookup(Key, T);
lookup(_, [])			  -> error.



unconsult(File, L) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
	file:close(S).


random_seed() ->
	?DEBUG("random_seed/2"),
	X = erlang:system_time(),
	{H,M,S} = time(),
	H1 = H * X rem 32767,
	M1 = M * X rem 32767,
	S1 = S * X rem 32767,
	put(random_seed, {H1,M1,S1}).


odds_and_evens(L) ->
	Odds  = [X || X <- L, (X rem 2) =:= 1], 
	Evens = [X || X <- L, (X rem 2) =:= 0],
	{Odds, Evens}.



odds_and_evens_acc(L) ->
	odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
	case (H rem 2) of
	1 -> odds_and_evens_acc(T, [H|Odds], Evens);
	0 -> odds_and_evens_acc(T, Odds, [H|Evens])
	end;
odds_and_evens_acc([], Odds, Evens) ->
	{Odds, Evens}.



sum(L) -> sum(L, 0).

sum([], N)	-> N;
sum([H|T], N) -> sum(T, H+N).



sqrt(X) when X < 0 ->	
	erlang:error({squareRootNegativeArgument, X});
sqrt(X) ->
	math:sqrt(X).



pmap(F, L) -> 
	S = self(),
	%% make_ref()は一意なリファレンスを返す
	%%   このリファレンスは後で照合に使う
	Ref = erlang:make_ref(), 
	Pids = map(fun(I) -> 
			   spawn(fun() -> do_f(S, Ref, F, I) end)
		   end, L),
	%% 結果を収集する
	gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->						
	Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
	receive
	{Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
	end;
gather([], _) ->
	[].

pmap1(F, L) -> 
	S = self(),
	Ref = erlang:make_ref(),
	foreach(fun(I) -> 
			spawn(fun() -> do_f1(S, Ref, F, I) end)
		end, L),
	%% 結果を収集する
	gather1(length(L), Ref, []).

do_f1(Parent, Ref, F, I) ->						
	Parent ! {Ref, (catch F(I))}.

gather1(0, _, L) -> L;
gather1(N, Ref, L) ->
	receive
	{Ref, Ret} -> gather1(N-1, Ref, [Ret|L])
	end.

foreachWordInFile(File, F) ->
	case file:read_file(File) of
	{ok, Bin} -> foreachWordInString(binary_to_list(Bin), F);
	_		 -> void
	end.



foreachWordInString(Str, F) ->
	case get_word(Str) of
	no -> 
		void;
	{Word, Str1} ->
		F(Word),
		foreachWordInString(Str1, F)
	end.


isWordChar(X) when $A=< X, X=<$Z -> true;
isWordChar(X) when $0=< X, X=<$9 -> true;
isWordChar(X) when $a=< X, X=<$z -> true;
isWordChar(_)  -> false.

get_word([H|T]) ->
	case isWordChar(H) of
	true  -> collect_word(T, [H]);
	false -> get_word(T)
	end;
get_word([]) ->
	no.

collect_word([H|T]=All, L) ->
	case isWordChar(H) of
	true  -> collect_word(T, [H|L]);
	false -> {reverse(L), All}
	end;
collect_word([], L) ->
	{reverse(L), []}.


deliberate_error(A) ->
	bad_function(A, 12),
	lists:reverse(A).

bad_function(A, _) ->
	{ok, Bin} = file:open({abc,123}, A),
	binary_to_list(Bin).

deliberate_error1(A) ->
	bad_function(A, 12).

