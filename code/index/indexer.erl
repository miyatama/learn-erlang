-module(indexer).
-export([start/0, 
     stop/0, 
     search/1, 
     cold_start/0]).

-import(lists, [map/2]).

-define(DEBUG(S), io:fwrite("[DEBUG] indexer: " ++ S ++ "~n")).
-define(DEBUG(S, Args), io:fwrite("[DEBUG] indexer: " ++ S ++ "~n", Args)).

cold_start() ->
    ?DEBUG("cold_start/0"),
    indexer_server:cold_start(output_dir(),  dirs_to_index()).

output_dir() -> 
    ?DEBUG("output_dir/0"),
    "./bigIndex".
dirs_to_index() -> 
    ?DEBUG("dirs_to_index/0"),
    ["./erl.supported"].

start() ->
    ?DEBUG("start/0"),
    indexer_server:start(output_dir()),
    spawn_link(fun() -> worker() end).


search(Str) ->
    ?DEBUG("search/1"),
    indexer_server:search(Str).

stop() ->
    ?DEBUG("stop/0"),
    ?DEBUG("Scheduling a stop"),
    indexer_server:schedule_stop().


worker() ->
    ?DEBUG("worker/0"),
    possibly_stop(),
    case indexer_server:next_dir() of
    {ok, Dir} ->
        Files = indexer_misc:files_in_dir(Dir),
        index_these_files(Files),
        indexer_server:checkpoint(),
        possibly_stop(),
        sleep(10000),
        worker();
    done ->
        true
    end.


possibly_stop() ->
    case indexer_server:should_i_stop() of
    true ->
        ?DEBUG("Stopping"),
        indexer_server:stop(),
        exit(stopped);
        false ->
        void
    end.

index_these_files(Files) ->
    ?DEBUG("index_these_files/1"),
    Ets = indexer_server:ets_table(),
    OutDir = filename:join(indexer_server:outdir(), "index"),
    F1 = fun(Pid, File) -> indexer_words:words_in_file(Pid, File, Ets) end,
    F2 = fun(Key, Val, Acc) -> handle_result(Key, Val, OutDir, Acc) end,
    indexer_misc:mapreduce(F1, F2, 0, Files).

handle_result(Key, Vals, OutDir, Acc) ->
    ?DEBUG("handle_result/4"),
    add_to_file(OutDir, Key, Vals),
    Acc + 1.

add_to_file(OutDir, Word, Is) ->
    ?DEBUG("add_to_file/3"),
    L1 = map(fun(I) -> <<I:32>> end, Is),
    OutFile = filename:join(OutDir, Word),
    case file:open(OutFile, [write,binary,raw,append]) of
    {ok, S} ->
        file:pwrite(S, 0, L1),
        file:close(S);
    {error, E} ->
          exit({ebadFileOp, OutFile, E})
    end.

sleep(T) ->
    ?DEBUG("sleep/1"),
    receive
    after T -> true
    end.
