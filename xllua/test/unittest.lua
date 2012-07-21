
package.path = "../src/?.lua" .. package.path

xllua = {}
require 'xllua'
local stringit = xllua.stringit

function xllua.debug_print( s )
	print( s )
end

function xllua.excel4(...)
	local t = {...}
	print( stringit( t ) )
	return 0
end

function xllua.file_exists( f )
	return true
end

function xllua.dump(...)
	print( stringit( ... ) )
end
local dump = xllua.dump

function test( n, ... )
	dump( xllua.fn( n, {...} ) )
end

--dofile( "addin1.lua" )

xllua.open( "../examples/redis_client.lua" )


test( "rds.connect", "test" )
test( "rds.ping", "test" )
test( "rds.set", "test", "key1", "hello" )
test( "rds.get", "test", "key1" )




