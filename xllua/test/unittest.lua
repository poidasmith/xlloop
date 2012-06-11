
xllua = {}

local f = loadfile( "addin1.lua" )
f()

function xllua.debug_print( s )
	print( s )
end

function xllua.excel4(...)
	print( stringit(...) )
	return 1
end

xllua.open( "test " )
print( stringit( xllua.funs ) )

