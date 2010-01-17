-module(xlloop_server).
-export([start/2, behaviour_info/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).
-define(TIME_OUT, 1000).

start(Port, Module) when is_integer(Port) ->
	spawn(fun() -> listen(Port, Module) end). 
	
behaviour_info(callbacks) ->
	[{function, 3}].
	
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
	{Name, R1} = xloper_codec:decode(Data),
	data(Socket, R1, Module, Name).

data(Socket, Data, Module, NameOrVersion) when is_integer(NameOrVersion) ->
	{HasContext, R1} = xloper_codec:decode(Data),
	case HasContext of
		true -> 
			{Caller, R2} = xloper_codec:decode(R1),
			{SheetName, R3} = xloper_codec:decode(R2),
			Context = {context, Caller, SheetName},
			{FunName, R4} = xloper_codec:decode(R3),
			data(Socket, R4, Module, Context, FunName);
		false ->
			{FunName, R2} = xloper_codec:decode(R1),
			data(Socket, R2, Module, null, FunName)
	end;
data(Socket, Data, Module, NameOrVersion) ->
	data(Socket, Data, Module, null, NameOrVersion).

data(Socket, Data, Module, Context, Name) ->
	{Argc, Rest} = xloper_codec:decode(Data),
	Args = decode_args(Argc, [], Rest),
    XLoper = Module:function(Context, Name, Args),
    gen_tcp:send(Socket, xloper_codec:encode(XLoper)).

decode_args(Argc, Argv, _Data) when Argc == 0 -> Argv;
decode_args(Argc, Argv, Data) ->
	{Arg, Rest} = xloper_codec:decode(Data),
	decode_args(Argc-1, Argv ++ [Arg], Rest).
	

	
