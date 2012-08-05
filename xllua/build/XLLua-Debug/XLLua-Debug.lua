
package.path = "../src/?.lua;" .. package.path

local f, error = loadfile( "addins.lua" )
if f ~= nil then
	local res, err = pcall( f )
	if not res then
		xllua.debug_printf( "error: %s\n", stringit( err ) )
	end
else
	xllua.debug_printf( "error: %s\n", error )
end 
