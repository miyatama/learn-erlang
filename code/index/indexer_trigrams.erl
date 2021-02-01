-module(indexer_trigrams).
-export([for_each_trigram_in_the_english_language/2,
     close/1, 
     howManyTrigrams/0, 
     is_word/2,
     lookup_all_dict/2,
     lookup_all_ets/2, 
     makeSet/0,
     make_dict/0, 
     make_ordered_set/0, 
     make_tables/0, 
     open/0, 
     timer_tests/0]).
-import(lists, [reverse/1]).

-define(DEBUG(S), io:fwrite("[DEBUG] indexer_trigram: " ++ S ++ "~n")).
-define(DEBUG(S, Args), io:fwrite("[DEBUG] indexer_trigram: " ++ S ++ "~n", Args)).

make_tables() ->  
    ?DEBUG("make_tables/0"),
    ?DEBUG("Building trigrams -- make take some time"),
    makeSet().

make_ordered_set() -> 
    ?DEBUG("make_ordered_set/0"),
    makeAset(ordered_set, "trigramsOS.tab").

makeSet() -> 
    ?DEBUG("makeSet/0"),
    makeAset(set, "trigramsS.tab").

makeAset(Type, FileName) ->        
    ?DEBUG("makeAset/0"),
    ?DEBUG("makeAset() - Filename: ~p", [FileName]),
    Tab = ets:new(table, [Type]),
    F = fun(Str, _) -> ets:insert(Tab, {list_to_binary(Str)}) end,
    for_each_trigram_in_the_english_language(F, 0),
    ets:tab2file(Tab, FileName),
    Size = ets:info(Tab, size),
    ets:delete(Tab),
    Size.

make_dict() ->
    ?DEBUG("make_dict/0"),
    D = dict:new(),
    F = fun(Str, Dict) -> dict:store(list_to_binary(Str),[],Dict) end,
    D1 = for_each_trigram_in_the_english_language(F, D),
    file:write_file("trigrams.dict", [term_to_binary(D1)]).

timer_tests() ->
    ?DEBUG("timer_tests/0"),
    time_lookup_set("Ordered Set", "trigramsOS.tab"),
    time_lookup_set("Set", "trigramsS.tab"),
    time_lookup_dict().

time_lookup_set(Type, File) ->
    ?DEBUG("time_lookup_set/2"),
    {ok, Tab} = ets:file2tab(File),
    L = ets:tab2list(Tab),
    Size = length(L),
    {M, _} = timer:tc(?MODULE, lookup_all_ets, [Tab, L]),
    ?DEBUG("~s lookup=~p micro seconds",[Type, M/Size]),
    ets:delete(Tab).

lookup_all_ets(Tab, L) ->
    ?DEBUG("lookup_all_ets/2"),
    lists:foreach(fun({K}) -> ets:lookup(Tab, K) end, L).

time_lookup_dict() ->
    ?DEBUG("time_lookup_dict/0"),
    {ok, Bin} = file:read_file("trigrams.dict"),
    Dict = binary_to_term(Bin),
    Keys = [Key || {Key,_} <- dict:to_list(Dict)],
    Size = length(Keys),
    {M, _} = timer:tc(?MODULE, lookup_all_dict, [Dict, Keys]),
    ?DEBUG("Dict lookup=~p micro seconds~n",[M/Size]).

lookup_all_dict(Dict, L) ->
    ?DEBUG("lookup_all_dict/0"),
    lists:foreach(fun(Key) -> dict:find(Key, Dict) end, L).


howManyTrigrams() ->
    ?DEBUG("howManyTrigrams/0"),
    F = fun(_, N) -> 1 + N  end,
    for_each_trigram_in_the_english_language(F, 0).
    
for_each_trigram_in_the_english_language(F, A0) ->
    ?DEBUG("for_each_trigram_in_the_english_language/2"),
    {ok, Bin0} = file:read_file("../354984si.ngl.gz"),
    Bin = zlib:gunzip(Bin0),
    scan_word_list(binary_to_list(Bin), F, A0).

scan_word_list([], _, A) ->
    ?DEBUG("scan_word_list/3"),
    A;
scan_word_list(L, F, A) ->
    ?DEBUG("scan_word_list/3"),
    {Word, L1} = get_next_word(L, []),
    A1 = scan_trigrams([$\s|Word], F, A),
    scan_word_list(L1, F, A1).

get_next_word([$\r,$\n|T], L) -> 
    ?DEBUG("get_next_word/2"),
    {reverse([$\s|L]), T};
get_next_word([H|T], L) -> 
    ?DEBUG("get_next_word/2"),
    get_next_word(T, [H|L]);
get_next_word([], L) -> 
    ?DEBUG("get_next_word/2"),
    {reverse([$\s|L]), []}.

scan_trigrams([X,Y,Z], F, A) ->
    ?DEBUG("scan_trigrams/3"),
    F([X,Y,Z], A);
scan_trigrams([X,Y,Z|T], F, A) ->
    ?DEBUG("scan_trigrams/3"),
    A1 = F([X,Y,Z], A),
    scan_trigrams([Y,Z|T], F, A1);
scan_trigrams(_, _, A) ->
    ?DEBUG("scan_trigrams/3"),
    A.

is_word(Tab, Str) -> 
    ?DEBUG("is_word/2"),
    is_word1(Tab, "\s" ++ Str ++ "\s").

is_word1(Tab, [_,_,_]=X) -> 
    ?DEBUG("is_word1/2"),
    is_this_a_trigram(Tab, X);

is_word1(Tab, [A,B,C|D]) ->
    ?DEBUG("is_word1/2"),
    case is_this_a_trigram(Tab, [A,B,C]) of
    true  -> is_word1(Tab, [B,C|D]);
    false -> false
    end;

is_word1(_, _) ->
    ?DEBUG("is_word1/2"),
    false.

is_this_a_trigram(Tab, X) ->
    ?DEBUG("is_this_a_trigram/2"),
    case ets:lookup(Tab, list_to_binary(X)) of
    [] -> false;
    _  -> true
    end.

open() ->
    ?DEBUG("is_this_a_trigram/2"),
    % TabFileName = filename:dirname(code:which(?MODULE)) ++ "/trigramsS.tab",
    TabFileName = "./trigramsS.tab",
    ?DEBUG("is_this_a_trigram() - tab filename: ~p", [TabFileName]),
    {ok, I} = ets:file2tab(TabFileName),
    I.

close(Tab) ->
    ?DEBUG("close/1"),
    ets:delete(Tab).

