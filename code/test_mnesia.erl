-module(test_mnesia).
-import(lists, [foreach/2]).

-include_lib("stdlib/include/qlc.hrl").

-export([do_this_once/0,
	start/0,
	demo/1,
	do/1,
	example_tables/0,
	add_shop_item/3,
	remove_shop_item/1,
	farmer/1,
	reset_tables/0,
	add_plans/0,
	get_plan/1]).

-define(DEBUG(S), io:fwrite("[DEBUG] test_mnesia: " ++ S ++ "~n")).


-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).
-record(design, {id, plan}).

do_this_once() ->
	?DEBUG("do_this_once/0"), 
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
	mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
	mnesia:create_table(design, [{attributes, record_info(fields, design)}]),
	mnesia:stop().

start() ->
	?DEBUG("start/0"), 
	mnesia:start(),
	mnesia:wait_for_tables([shop, cost, design], 20000).

% select * from shop
demo(select_shop) ->
	?DEBUG("demo/1"),
	do(qlc:q([X || X <- mnesia:table(shop)]));
% select item, quantity from shop
demo(select_some) ->
	?DEBUG("demo/1"),
	do(qlc:q([{X#shop.item, X#shop.quantity} || 
		  X <- mnesia:table(shop)]));
% select shop.item from shop where shop.quantity < 250
demo(reorder) ->
	?DEBUG("demo/1"),
	do(qlc:q([X#shop.item || 
		  X <- mnesia:table(shop),
		 X#shop.quantity < 250]));
% select shop.item, shop.quantity, cost.name, cost.price from shop, cost where shop.item = cost.name and cost.price < 2 and shop.quantity < 250
demo(join) ->
	?DEBUG("demo/1"),
	do(qlc:q([{X#shop.item, X#shop.quantity, Y#cost.name, Y#cost.price} ||
		  X <- mnesia:table(shop),
		  X#shop.quantity < 250,
		  Y <- mnesia:table(cost),
		  X#shop.item =:= Y#cost.name,
       		  Y#cost.price < 2])).

do(Q) ->
	?DEBUG("do/1"),
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

example_tables() ->
	?DEBUG("example_tables/0"),
	[% shop
	 {shop, apple, 20, 2.3},
	 {shop, orange, 100, 3.8},
	 {shop, pear, 200, 3.6},
	 {shop, banana, 420, 4.5},
	 {shop, potato, 2456, 1.2},
	 % cost
	 {cost, apple, 1.5},
	 {cost, orange, 2.4},
	 {cost, pear, 2.2},
	 {cost, banana, 1.5},
	 {cost, potato, 0.6}
	].

add_shop_item(Name, Quantity, Cost) ->
	?DEBUG("add_shop_item/3"),
	Row = #shop{item=Name, quantity=Quantity, cost=Cost},
	F = fun() ->
		mnesia:write(Row)
	    end,
	mnesia:transaction(F).

remove_shop_item(Item) ->
	?DEBUG("remove_shop_item/1"),
	Old = {shop, Item},
	F = fun() ->
		mnesia:delete(Old) 
	    end,
	mnesia:transaction(F).

farmer(Nwant) ->
	?DEBUG("farmer/1"),
	F = fun() ->
			    [Apple] = mnesia:read({shop, apple}),
			    Napples = Apple#shop.quantity,
			    Apple1 = Apple#shop{quantity = Napples + 2*Nwant},
			    mnesia:write(Apple1),
			    [Orange] = mnesia:read({shop, orange}),
			    NOrange = Orange#shop.quantity,
			    if
				    NOrange >= Nwant ->
					    N1 = NOrange - Nwant,
					    Orange1 = Orange#shop{quantity = N1},
					    mnesia:write(Orange1);
				    true -> 
					    mnesia:abort(oranges)

			    end
	    end,
	mnesia:transaction(F).

reset_tables() ->
	?DEBUG("reset_tables/0"),
	mnesia:clear_table(shop),
	mnesia:clear_table(cost),
	F = fun() ->
			    foreach(fun mnesia:write/1, example_tables())
	    end,
	mnesia:transaction(F).

add_plans() ->
	?DEBUG("add_plans/0"),
	D1 = #design{id = {joe, 1},
		    plan = {circle, 10}},
	D2 = #design{id = fred,
		    plan = {rectangle, 10, 5}},
	D3 = #design{id = {jane, {house, 23}},
		    plan = [{floor, 1,
			     [{doors, 3},
			      {windows, 12},
			      {rooms, 5}]},
			     {floor, 2,
			      [{doors, 2},
			       {rooms, 4}]}]},
	F = fun() ->
			    mnesia:write(D1),
			    mnesia:write(D2),
			    mnesia:write(D3)
	    end,
	mnesia:transaction(F).

get_plan(PlanId) ->
	?DEBUG("get_plan/1"), 
	F = fun() ->
			    mnesia:read({design, PlanId})
	    end,
	mnesia:transaction(F).
