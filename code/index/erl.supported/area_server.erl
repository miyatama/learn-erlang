-module(area_server).

-behavior(gen_server).

-export([area/1, start_link/0]).
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).
-define(DEBUG(S), io:fwrite("[DEBUG] area_server: " ++ S ++ "~n")).

start_link() ->
	?DEBUG("start_link/0"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

area(Thing) ->
	?DEBUG("area/1"),
	gen_server:call(?MODULE, {area, Thing}).

init([]) ->
	?DEBUG("init/1"),
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	{ok, 0}.

handle_call({area, Thing}, _From, N) ->
	?DEBUG("handle_call/3"),
	{reply, compute_area(Thing), N+1}.

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

compute_area({square, X}) ->
	?DEBUG("compute_area/1"),
	X*X;
compute_area({rectangle, X, Y}) ->
	?DEBUG("compute_area/1"),
	X*Y.
