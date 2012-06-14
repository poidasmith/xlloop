
xllua = {}
dofile( "../src/xllua.lua" )
local stringit = xllua.stringit

function xllua.debug_print( s )
	print( s )
end

function xllua.excel4(...)
	local t = {...}
	print( stringit( t ) )
	return 0
end

--dofile( "addin1.lua" )

xllua.open( "test " )
print( stringit( xllua.funs ) )

res = xllua.fn( "Test", "hello" )
print( stringit( res ) )
res = xllua.fc( 0, "test" )
print( stringit( res ) )


