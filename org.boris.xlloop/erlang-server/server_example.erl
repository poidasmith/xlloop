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
	
function(_Name, _Args) ->
	{string, "Hello World!"}.
	
