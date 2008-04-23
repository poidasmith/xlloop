-module(server_example).
-export([start/0, stop/0, request/2, function/2]).
-behaviour(xlloop_server).

start() ->
	register(sep, xlloop_server:start(5454, server_example)).
	
stop() ->
	sep ! stop.

request(_Name, _Args) ->
	{collection, []}.
	
function(Name, _Args) ->
	{string, "Hello World!"}.
	
