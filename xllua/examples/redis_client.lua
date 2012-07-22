
package.path  = package.path .. ";F:/Development/Lua/5.1/lua/?.lua"
package.cpath = package.cpath .. ";F:/Development/Lua/5.1/clibs/?.dll"

xllua.debug_printf( "%s\n%s\n", package.path, package.cpath );

local redis = require 'redis'

-- named connections

local conns = {}

local function connection(args)
	name, host, port = unpack(args)
	local c = conns[name]
	if c ~= nil then
		c.c:quit()
	end
		
	host = host or "localhost"
	port = port or 6379
	
	conns[name] = {
		host = host,
		port = port,
		c    = redis.connect(host, port),
	}
		
	return string.format("%s@%s:%d", name, host, port)
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

fns = {}

for k, v in pairs(redis.commands) do
	fns["rds." .. k] = stdf(k)
end

fns["rds.connect"] = connection
 
xllua.reg_funs( fns ) 

xllua.debug_printf( "xlredis...\n" )
