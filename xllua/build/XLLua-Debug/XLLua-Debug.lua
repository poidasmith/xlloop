--[[ ***************************************************************************
* 
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
******************************************************************************]]

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
