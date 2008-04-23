-module(server_example).
-export([start/0, stop/0, request/2, function/2]).
-behaviour(xlloop_server).
-define(PORT, 5454).

start() ->
	Pid = xlloop_server:start(?PORT, ?MODULE),
	register(server_example_pid, Pid).
	
stop() ->
	server_example_pid ! stop.

request(_Name, _Args) ->
	{collection, []}.
	
function(Name, Args) ->
	case Name of
		{string, "Sum"} -> sum_list(Args);
		_ -> {string, "Hello World!"}
	end.
		
sum_list(Args) ->
	{collection, [First|_Rest]} = Args,
	case First of
		{collection, L} ->
			Fun = fun(Elem, AccIn) ->
				case Elem of
					{double, D} -> AccIn + D;
					_ -> AccIn
				end
			end,
			{double, lists:foldl(Fun, 0, L)};
		_ -> {double, 0}
	end.
				
		