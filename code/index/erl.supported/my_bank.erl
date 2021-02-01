-module(my_bank).

-behavior(gen_server).
-export([start/0,
	stop/0,
	new_account/1,
	deposit/2,
	withdraw/2]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).
-define(DEBUG(S), io:fwrite("[DEBUG] my_bank: " ++ S ++ "~n")).

%% gen_server_template callbacks
init([]) -> 
	?DEBUG("init/1"),
	{ok, ets:new(?MODULE, [])}.

handle_call({new, Who}, _From, Tab) ->
	?DEBUG("handle_call/3"),
	Reply = case ets:lookup(Tab, Who) of
			[] -> ets:insert(Tab, {Who, 0}),
			      {welcome, Who};
			[_] -> {Who, you_already_are_a_customer}
		end,
	{reply, Reply, Tab};
handle_call({add, Who, X}, _From, Tab) ->
	?DEBUG("handle_call/3"),
	Reply = case ets:lookup(Tab, Who) of
			[] -> not_a_customer;
			[{Who, Balance}] ->
				NewBalance = Balance + X,
				ets:insert(Tab, {Who, NewBalance}),
				{thanks, Who, your_balance_is, NewBalance}
		end,
	{reply, Reply, Tab};
handle_call({remove, Who, X}, _From, Tab) ->
	?DEBUG("handle_call/3"),
	Reply = case ets:lookup(Tab, Who) of 
			[] -> not_a_customer;
			[{Who, Balance}] when X =< Balance ->
				NewBalance = Balance - X,
				ets:insert(Tab, {Who, NewBalance}),
				{thanks, Who, your_balance_is, NewBalance};
			[{Who, Balance}]  ->
				{sorry, Who, you_only_have, Balance, in_the_bank}
		end,
	{reply, Reply, Tab};
handle_call(stop, _From, Tab) ->
	?DEBUG("handle_call/3"),
	{stop, onrmal, stoppped, Tab}.

handle_cast(_Msg, State) ->
	?DEBUG("handle_cast/2"),
	{noreply, State}.

handle_info(_Info, State) ->
	?DEBUG("handle_info/2"),
	{noreply, State}.

terminate(_Reason, _State) ->
	?DEBUG("terminate/2"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	?DEBUG("code_change/3"),
	{ok, State}.


start() ->
	?DEBUG("start/0"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	?DEBUG("stop/0"),
	gen_server:call(?MODULE, stop).

new_account(Who) ->
	?DEBUG("new_account/1"),
	gen_server:call(?MODULE, {new, Who}).

deposit(Who, Amount) ->
	?DEBUG("deposit/2"),
	gen_server:call(?MODULE, {add, Who, Amount}).

withdraw(Who, Amount) ->
	?DEBUG("withdraw/2"),
	gen_server:call(?MODULE, {remove, Who, Amount}).
