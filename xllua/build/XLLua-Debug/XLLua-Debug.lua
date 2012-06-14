

local function myfunc( args )
	xllua.debug_printf( "myfunc: %s\n", xllua.stringit( args ) )
	xllua.debug_printf( "to_table: %s\n", xllua.stringit( xllua.to_table( args[1] ) ) );
	return "yo"
end

local function wallaby( args )
	xllua.debug_printf( "wallaby: %s\n", xllua.stringit( args ) )
	return "why"
end


xllua.reg_fun( "MyFunc",  "Lua", myfunc )
xllua.reg_fun( "Wallaby", "Lua", wallaby )

xllua.debug_printf( "I am in the addin man!\n" )

return 67