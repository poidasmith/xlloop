
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

function xllua.file_exists( f )
	return true
end

--dofile( "addin1.lua" )

xllua.open( "../build/XLLua-Debug/XLLua-Debug.xll" )
print( stringit( xllua.funs ) )

res = xllua.fn( "Test", "hello" )
print( stringit( res ) )
res = xllua.fc( 0, "test" )
print( stringit( res ) )

print( stringit( xllua.fn( "Ping") ) )


