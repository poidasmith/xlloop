-module(server_example).
-export([start/0, stop/0, function/3]).
-behaviour(xlloop_server).
-define(PORT, 5454).

start() ->
	Pid = xlloop_server:start(?PORT, ?MODULE),
	register(server_example_pid, Pid).
	
stop() ->
	server_example_pid ! stop.

function(_Context, _Name, _Args) ->
	{str, "Hello World!"}.
		
