# XLLua - xll, excel addin framework for Lua

XLLua is a framework for writing your own excel user-defined functions (UDF's) in Lua.

It allows the developer to:

*   Easily extend Excel by writing Lua functions that can be exposed as Excel functions
*   Use Excel as an application prototype framework (call lua functions, build tables)

## How to use XLLua

First download the latest version of the addin (XLLua-version.xll) at: 
 https://github.com/poidasmith/xlloop/downloads
 
When the addin is loaded by Excel it will search for a Lua file with the same name as the addin
and in the same directory (with just the extension as .lua instead of .xll)

The addin will call dofile on this script during initialization. All function
registration should occur at this point.

A simple addin that registers a new Excel function called 'MyFunc' is just:

<pre>
-- set debug output on (use dbgview tool from sysinternals to view logging)
xllua.options.debug = true

local function myfunc( args )
	xllua.debug_printf( "myfunc: %s\n", xllua.stringit( args ) )
	xllua.debug_printf( "to_table: %s\n", xllua.stringit( xllua.to_table( args[1] ) ) );
	return "Hello Excel from Lua!"
end

-- register the above function in Excel 
xllua.reg_funs( {
	MyFunc  = myfunc,
} )

-- log something so we can check that our addin has been loaded
xllua.debug_printf( "testing testing...\n" )

return 1 
</pre>

	
