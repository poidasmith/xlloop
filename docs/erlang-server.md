# ![XLLoop](./logo.gif) XLLoop 
An Erlang Function Server

## About
Included in the download is an erlang implementation of the xlloop server process. 

For more information on Erlang you can try:
* Erlang.org for downloads and reference material
* Trapexit.org for cookbooks, wiki etc..

## Usage
The erlang server implementation consists of three files:
* `variant_codec.erl` - this is responsible for marshalling the excel objects.
* `xlloop_server.erl` - this is the server framework that dispatches requests/function calls to your server implementation.
* `server_example.erl` - an example server implementation

The code listing for the example server is as follows:

```erlang
-module(server_example).
-export([start/0, stop/0, function/2]).
-behaviour(xlloop_server).
-define(PORT, 5454).

start() ->
	Pid = xlloop_server:start(?PORT, ?MODULE),
	register(server_example_pid, Pid).
	
stop() ->
	server_example_pid ! stop.

function(_Name, _Args) ->
	{str, "Hello World!"}.
```

This creates a new server (a socket listening on port 5454). The main function is "function". This is called whenever excel invokes a function.