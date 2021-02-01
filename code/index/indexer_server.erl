-module(indexer_server).

-export([cold_start/2, 
     start/1, 
     filename2index/1, 
     next_dir/0,
     ets_table/0, 
     checkpoint/0,
     schedule_stop/0,
     search/1,
     should_i_stop/0,
     outdir/0, 
     stop/0]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-import(filename, [join/2]).

-record(env,{ets, cont, nextCP, outdir, stop=false}).

-define(DEBUG(S), io:fwrite("[DEBUG] indexer_server: " ++ S ++ "~n")).
-define(DEBUG(S, Args), io:fwrite("[DEBUG] indexer_server: " ++ S ++ "~n", Args)).

start(Dir) ->
    ?DEBUG("start/1"),
    ?DEBUG("starting ~p ~p",[?MODULE, Dir]),
    gen_server:start({local,?MODULE}, ?MODULE, Dir, []).

schedule_stop() ->
    ?DEBUG("schedule_stop/0"),
    gen_server:call(?MODULE, schedule_stop).

should_i_stop() ->
    ?DEBUG("should_i_stop/0"),
    gen_server:call(?MODULE, should_i_stop).

stop() ->
    ?DEBUG("stop/0"),
    gen_server:cast(?MODULE, stop).

filename2index(File) ->  
    ?DEBUG("filename2index/1"),
    gen_server:call(?MODULE, {f2i, File}).

next_dir() -> 
    ?DEBUG("next_dir/0"),
    gen_server:call(?MODULE, next_dir).

checkpoint() -> 
    ?DEBUG("checkpoint/0"),
    gen_server:call(?MODULE, checkpoint).

outdir() -> 
    ?DEBUG("outdir/0"),
    gen_server:call(?MODULE, outdir).

ets_table() -> 
    ?DEBUG("ets_table/0"),
    gen_server:call(?MODULE, ets_table).    

search(Str) -> 
    ?DEBUG("search/0"),
    gen_server:call(?MODULE, {search, Str}).

cold_start(OutputDir, SearchDirs) ->
    ?DEBUG("cold_start/2"),
    case file:list_dir(OutputDir) of
        {ok, []} ->
            OutputDir1 = join(OutputDir,"index"),
            ?DEBUG("output dir ~p", [OutputDir1]), 
            ?DEBUG("Creating ~p",[OutputDir1]),
            file:make_dir(OutputDir1),
            Cont = indexer_dir_crawler:start(SearchDirs),
            Check = {OutputDir, Cont},
            ?DEBUG("creating checkpoint:~p",[Check]),
            indexer_checkpoint:init(OutputDir, Check);
        _ ->
            exit({eDirNotEmptyOrMissing, OutputDir})
    end.

init(Dir) ->
    ?DEBUG("init/1"),
    ?DEBUG("restarting:~p", [Dir]),
    {Next, {OutDir, Cont}} = indexer_checkpoint:resume(Dir),
    ?DEBUG("resume with:~p ~p",[Next, Cont]),
    indexer_filenames_dets:open(join(Dir,"filenames.dets")),
    ?DEBUG("opening trigrams"),
    Tab = indexer_trigrams:open(),
    {ok, #env{ets = Tab, outdir=OutDir, cont=Cont, nextCP = Next}}.

handle_call({f2i, File}, _From, S) ->
    ?DEBUG("handle_call/3 - f2i"),
    B = list_to_binary(File),
    {reply, indexer_filenames_dets:filename2index(B), S};

handle_call(ets_table, _From, S) ->
    ?DEBUG("handle_call/3 - ets_table"),
    {reply, S#env.ets, S};

handle_call(next_dir, _From, S) ->
    ?DEBUG("handle_call/3 - next_dir"),
    Cont = S#env.cont,
    case indexer_dir_crawler:next(Cont) of
        {dir, Dir, _} ->
            {reply, {ok, Dir}, S};
        done ->
            {reply, done, S}
    end;

handle_call(checkpoint, _From, S) ->
    ?DEBUG("handle_call/3 - checkpoint"),
    Cont = S#env.cont,
    case indexer_dir_crawler:next(Cont) of
        {dir, _Dir, Cont1} ->
            Next = S#env.nextCP,
            OutDir = S#env.outdir,
            Next1 = indexer_checkpoint:checkpoint(Next, {OutDir, Cont1}),
            S1 = S#env{nextCP = Next1, cont=Cont1},
            {reply, ok, S1};
        done ->
            {reply, done, S}
    end;

handle_call(schedule_stop, _From, S) ->
    ?DEBUG("handle_call/3 - schedule_stop"),
    {reply, ack, S#env{stop=true}};

handle_call({search, Str}, _From,S) ->
    ?DEBUG("handle_call/3 - search"),
    Result = indexer_misc:search(Str, S#env.outdir, S#env.ets),
    {reply, Result, S};

handle_call(should_i_stop, _From, S) ->
    ?DEBUG("handle_call/3 - should_i_stop"),
    {reply, S#env.stop, S};

handle_call(outdir, _From, S) ->
    ?DEBUG("handle_call/3 - outdir"),
    {reply, S#env.outdir, S}.

handle_cast(stop, S) ->
    ?DEBUG("handle_cast/2"),
    {stop, normal, S}.

terminate(Reason, S) ->
    ?DEBUG("terminate/2"),
    Ets = S#env.ets,
    indexer_trigrams:close(Ets),
    indexer_filenames_dets:close(),
    ?DEBUG("stopping ~p",[Reason]).
