-module(xlloop_server).
-export([start/2, behaviour_info/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).
-define(TIME_OUT, 1000).

start(Port, Module) when is_integer(Port) ->
	spawn(fun() -> listen(Port, Module) end). 
	
behaviour_info(callbacks) ->
	[{function, 2}].
	
listen(Port, Module) ->
	{ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(Socket, Module).
	
accept(ServerSocket, Module) ->
	{A, B} = gen_tcp:accept(ServerSocket, ?TIME_OUT),
	case {A, B} of
		{ok, Socket} -> 
			spawn(fun() -> handle(Socket, Module) end),
			accept(ServerSocket, Module);
		{error, timeout} ->
			receive
				stop -> {ok, stopped}
			after 
				0 -> accept(ServerSocket, Module)
			end
	end.
	
handle(Socket, Module) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} -> 
			data(Socket, Data, Module),
			handle(Socket, Module);
		{error, closed} -> {error}
	end.
	
data(Socket, Data, Module) ->
	{Name, Rest2} = xloper_codec:decode(Rest),
	{Argc, Rest3} = xloper_codec:decode(Rest2),
	{Args, _Rest3} = xloper_codec:decode(Rest2),
    XLoper = Module:function(Name, Args),
    gen_tcp:send(Socket, xloper_codec:encode(XLoper)).
		
	
	
