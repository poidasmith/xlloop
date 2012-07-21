
xllua.options.debug = true

local function myfunc( args )
	xllua.debug_printf( "myfunc: %s\n", xllua.stringit( args ) )
	xllua.debug_printf( "to_table: %s\n", xllua.stringit( xllua.to_table( args[1] ) ) );
	return "myfunc in Lua"
end

local function wallaby( args )
	xllua.debug_printf( "wallaby: %s\n", xllua.stringit( args ) )
	return { rows = 3, cols = 2, "why", "is", "this", "so", "easy?" }
end

local store = {}

local function lput( args )
	local n = args[1] or ""
	local v = args[2]
	store[ n ]  = v 
	return n
end

local function lget( args )
	local n = args[1] or ""
	return store[ n ]
end

local function dynecho( args )
	return args
end

xllua.reg_funs( {
	MyFunc  = myfunc,
	Wallaby = wallaby,
	lput    = lput,
	lget    = lget,
} )

xllua.reg_dyns( {
	dynecho = dynecho,
} )

xllua.debug_printf( "testing testing...\n" )

return 1