
local store = {}

local function myfunc( args )
	xllua.debug_printf( "myfunc: %s\n", xllua.stringit( args ) )
	xllua.debug_printf( "to_table: %s\n", xllua.stringit( xllua.to_table( args[1] ) ) );
	return "yo"
end

local function wallaby( args )
	xllua.debug_printf( "wallaby: %s\n", xllua.stringit( args ) )
	return { rows = 3, cols = 2, "why", "is", "this", "so", "easy?" }
end

xllua.reg_fun( "MyFunc",  "Lua", myfunc )
xllua.reg_fun( "Wallaby", "Lua", wallaby )
xllua.reg_fun( "lput", "Lua", lput )

xllua.debug_printf( "I am in the addin man!\n" )

return 67