# XLLua Example Scripts

## Redis Client

An example using the lua redis client (https://github.com/nrk/redis-lua). 

The example script has two parts:
*    A connection manager that allows named connections to be used in redis commands
*    A function wrapper that registers each redis command with a function that takes
     the named connection as the first argument.
     
To use the redis client in Excel you could do something like

<pre>
A1 =rds.connect("test", "127.0.0.1", 6379)
A2 =rds.set("test", "mykey", "helo redis")
A3 =rds.get("test", "mykey")
</pre>

## CSV Utilities

An example that provides a collection of functions for working with CSV files

Available functions are:
*    Read a CSV file (or part of) and return as an array
*    Read the header line of a CSV file and return as an array
*    Count the number of rows/columns in a CSV file 

## Windows System Information

An example that provides a collections of functions for viewing system information

Available functions are:
*    View current processes and details; CPU, memory use
*    View all system services and details (startup, running stat)
*    