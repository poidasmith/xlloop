# XLLua Example Scripts

## Redis Client

An example using the lua redis client (https://github.com/nrk/redis-lua). 

The example script has two parts:
*    A connection manager that allows named connections to be used in redis commands
*    A function wrapper that registers each redis command with a function that takes
     the named connection as the first argument.
     
To use the redis client in Excel you could do something like

<pre>
A1 =rds.connect("test", "127.0.0.1", 6739)
A2 =rds.set("test", "mykey", "helo redis")
A3 =rds.get("test", "mykey")
</pre>
