-module(xlloop_server).
-export([start/1,stop/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).

stop() ->
	get(server_pid) ! stopped.
	
start(Port) when is_integer(Port) ->
	put(server_pid, spawn(fun() -> listen(Port) end)).
	
listen(Port) ->
	put(state, running),
	{ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(Socket).
	
accept(ServerSocket) ->
	{ok, Socket} = gen_tcp:accept(ServerSocket),
	receive
		stopped -> gen_tcp:close(Socket), {ok, stopped}
	after 
		0 -> spawn(fun() -> handle(Socket) end), accept(ServerSocket)
	end.
	
handle(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} -> data(Socket, Data);
		{error, closed} -> {error}
	end.
	
data(Socket, Data) ->
	{Variant, _Rest} = variant_codec:decode(Data),
	Res = function(Variant),
	Type = variant_codec:encode({long, 1}),
	gen_tcp:send(Socket, Type),
	Bin = variant_codec:encode(Res),
	gen_tcp:send(Socket, Bin),
	handle(Socket).
		
function(_Variant) ->
	{string, "Hello World!"}.
