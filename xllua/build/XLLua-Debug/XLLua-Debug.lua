
package.path  = package.path .. ";F:/Development/Lua/5.1/lua/?.lua"
package.cpath = package.cpath .. ";F:/Development/Lua/5.1/clibs/?.dll"

--xllua.debug_printf( "PATH: %s\n", os.getenv( "PATH" ) )
--xllua.debug_printf( "package path: %s\n", package.path )
--xllua.debug_printf( "package cpath: %s\n", package.cpath )

local redis  = require 'redis'

local conns = {}
local function conn(name)
	local c = conns[name]
	if c == nil then
		return string.format("#Unknown server: %s", name)
	end 
	return c.c
end

local function connection(name, host, port)
	local c = conns[name]
	if c ~= nil then
		c.c:quit()
	end
		
	conns[name] = {
		host = host,
		port = port,
		c    = redis.connect(address, port),
	}
		
	return string.format("%s@%s:%d", name, host, port)
end	

-- KEYS

local function del(svr, keys)
	return conn(svr):pipeline(
		function(p)
			for i, v in ipairs(keys) do
				p:del(v)	
			end
		end
	)
end

local function dump(svr, key)
	return conn(key):dump(key)
end

local function exists(svr, key)
	return conn(svr):exists(key)
end

local function expire(svr, key, seconds)
	return conn(svr):expire(key, seconds)
end

local function expireat(svr, key, timestamp)
	return conn(svr):expireat(key, timestamp)
end

local function keys(svr, pattern)
	return conn(svr):keys(pattern)
end

local function migrate(svr, host, port, key, dest_db, timeout)
	return conn(svr):migrate(host, port, key, dest_db, timeout)
end

local function move(svr, key, db)
	return conn(svr):move(key, db)
end

local function pexpire(svr, key, millis)
	return conn(svr):pexpire(key, millis)
end

local function pexpireat(svr, key, millis_ts)
	return conn(svr):pexpireat(key, millis_ts)
end

local function pttl(svr, key)
	return conn(svr):pttl(key)
end

-- STRINGS

local function get(svr, key)
	return conn(svr):get(key)
end

local function set(svr, key, value)
	return conn(svr):set(key, value)
end

-- SERVER

local function ping(svr)
	return conn(svr):ping()
end

-- wrappers to unpack args

local unpk  = function(f) return function(args) return f(unpack(args)) end end
local unpkv = function(f) return function(args) return f(args.remove(1), args) end end 

fns = {
	-- SERVER
	["rds.connect"] = unpk(connection),
	["rds.ping"]    = unpk(ping),
	
	-- KEYS
	["rds.del"]       = unpkv(del),
	["rds.dump"]      = unpk(dump),
	["rds.exists"]    = unpk(rexists),
	["rds.epxire"]    = unpk(expire),
	["rds.expireat"]  = unpk(expireat),
	["rds.keys"]      = unpk(keys),
	["rds.migrate"]   = unpk(migrate),
	["rds.move"]      = unpk(move),
	["rds.object"]    = unpkv(object),
	["rds.pexpire"]   = unpk(pexpire),
	["rds.expireat"]  = unpk(pexpireat),
	["rds.pttl"]      = unpk(pttl),
	["rds.randomkey"] = unpk(randomkey),
	["rds.rename"]    = unpk(rename),
	["rds.renamenx"]  = unpk(renamenx),
	["rds.restore"]   = unpk(restore),
	["rds.sort"]      = unpkv(sort),
	["rds.ttl"]       = unpk(ttl),
	["rds.type"]      = unpk(type),
	
	["rds.get"] = unpk(get),
	["rds.set"] = unpk(set),
}

xllua.reg_funs( fns ) 

xllua.debug_printf( "xlredis is ready...\n" )
