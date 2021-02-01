-module(sellaprime_supervisor).
-behavior(supervisor).

-export([start/0,
	 start_in_shell_for_testing/0,
	 start_link/1,
	 init/1]).

-define(DEBUG(S), io:fwrite("[DEBUG] " ++ S ++ "~n")).

start() ->
	?DEBUG("start/0"),
	spawn(fun() ->
		      supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
	      end).

start_in_shell_for_testing() ->
	?DEBUG("start_in_shell_for_testing/0"),
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid).

start_link(Args) ->
	?DEBUG("start_link/1"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
	?DEBUG("init/1"),
	gen_event:swap_handler(alarm_handler,
			       {alarm_handler, swap},
			       {my_alarm_handler, xyz}),
	% MaxRestarts: 3
	% Time: 10
	{ok, 
	 {{one_for_one, 3, 10},
	  [{tag1,
		{area_server, start_link, []},
		 permanent, 
		 10000,
		 worker,
		 [area_server]},
	   {tag2,
		{prime_server, start_link, []},
		 permanent, 
		 10000,
		 worker,
		 [prime_server]}
	  ]}}.
