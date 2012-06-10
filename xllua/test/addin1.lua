

function xllua.debug_printf( fmt, ... )
	xllua.debug_print( string.format( fmt, ... ) )
end

function xllua.open()
	xllua.debug_printf( "xllua.opening...\n" )
	return 1
end

function xllua.close()
	xllua.debug_printf( "xllua.closing...\n" )
	return 1
end

function xlFn( name, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10 )
	xllua.debug_printf( "xllua.invoke: %s\n", tostring( { name, v1, v2, v3, v4, v5, v6, v7, v8, v9 } ) )
	return 1
end

function xlFc( num, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9 )
end