-module(xlloop_server).
-export([start/2, behaviour_info/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).
-define(TIME_OUT, 1000).
-define(REPLY_OK, "Ok").
-define(REPLY_ERROR, "Error").
-define(REQ_TYPE_GENERIC, 0).
-define(REQ_TYPE_FUNCTION, 1).

start(Port, Module) when is_integer(Port) ->
	spawn(fun() -> listen(Port, Module) end). 
	
behaviour_info(callbacks) ->
	[{request, 2}, {function, 2}].
	
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
	{Type, Rest} = variant_codec:decode(Data),
	{Name, Rest2} = variant_codec:decode(Rest),
	{Args, _Rest3} = variant_codec:decode(Rest2),
	case Type of
		{long, ?REQ_TYPE_GENERIC} -> request(Socket, Name, Args, Module);
		{long, ?REQ_TYPE_FUNCTION} -> function(Socket, Name, Args, Module)
	end.
		
function(Socket, Name, Args, Module) ->
	reply(Socket, ?REPLY_OK, Module:function(Name, Args)).
	
request(Socket, Name, Args, Module) ->
	reply(Socket, ?REPLY_OK, Module:request(Name, Args)).
	
reply(Socket, Type, Variant) ->
	gen_tcp:send(Socket, variant_codec:encode({string, Type})),
	gen_tcp:send(Socket, variant_codec:encode(Variant)).
	
