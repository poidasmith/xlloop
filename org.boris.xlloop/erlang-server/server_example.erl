-module(server_example).
-export([start/0, stop/0, request/2, function/2]).
-behaviour(xlloop_server).
-define(PORT, 5454).

start() ->
	Pid = xlloop_server:start(?PORT, ?MODULE),
	register(server_example_pid, Pid).
	
stop() ->
	server_example_pid ! stop.

request(Name, _Args) ->
	case Name of
	    % This part is used to register proper excel functions (not strictly necessary
	    %  but more user friendly 
		{string, "GetFunctions"} ->
			{collection, [{struct, [
				{{string, "functionName"}, {string, "Erl.Sum"}}, % The function name in excel
				{{string, "functionHelp"}, {string, "Sums a range"}},
				{{string, "category"}, {string, "Maths"}},
				{{string, "argumentText"}, {string, "Range"}},
				{{string, "argumentHelp"}, {collection, [
					{string, "The range to sum"}
				]}}]}]};
		_ -> {collection, []}
	end.
	
function(Name, Args) ->
	case Name of
		{string, "Erl.Sum"} -> sum_list(Args);
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
				
		