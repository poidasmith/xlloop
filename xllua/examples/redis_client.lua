--[[ ***************************************************************************
* 
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
******************************************************************************]]

-- A Redis client - connects to a redis server (or many) and can issue commands

package.path  = package.path .. ";F:/Development/Lua/5.1/lua/?.lua"
package.cpath = package.cpath .. ";F:/Development/Lua/5.1/clibs/?.dll"

xllua.debug_printf( "%s\n%s\n", package.path, package.cpath );

local os    = require 'os'
local redis = require 'redis'

-- named connections

local conns = {}

local function connection(args)
	name, host, port = unpack(args)
	
	host = host or "localhost"
	port = port or 6379
	
	local c = conns[name]
	if c ~= nil then
		c.c:quit()
	end
		
	conns[name] = {
		host = host,
		port = port,
		c    = redis.connect(host, port),
	}
		
	return string.format("%s@%s:%d (%f)", name, host, port, os.clock())
end	

-- redis command wrapper

local function stdf(name)
	return function(args)
		local svr = table.remove(args, 1)
		local c = conns[svr];
		if c == nil then
			return string.format("#Unknown server: %s", xllua.stringit( name ) )
		end 
		local f = c.c[name]
		return f(c.c, args)
	end	 
end

-- our redis commands, wrapped in a connection lookup function 

fns = {}

for k, v in pairs(redis.commands) do
	fns["rds." .. k] = stdf(k)
end

fns["rds.connect"] = connection

-- register all the redis commands with Excel

xllua.reg_funs( fns ) 

xllua.debug_printf( "xlredis...\n" )
