
function xllua.stringit( t ) -- should stick this somewhere common
	local res = ""
	if     type( t ) == "table" then
		local first = true
		res = "{"
		for i, v in pairs(t) do
			local sep = ", "
			if first then
				sep = " "
				first = false
			end
			res = res .. sep .. xllua.stringit( i ) .. "=" .. xllua.stringit( v ) 
		end
		res = res .. " }"
	elseif type( t ) == "string" then
		res = string.format( "%q", t )
	else
		res = tostring( t )		
	end
	return res
end

local stringit = xllua.stringit

local xlfRegister = 149

local procTypes = "RPPPPPPPPPPPPPPPPPPPP"

xllua.funs = { }

function xllua.debug_printf( fmt, ... )
	xllua.debug_print( string.format( fmt, ... ) )
end

function xllua.reg_fun( name, category, fn )
	local index   = #xllua.funs + 1
	local proc    = string.format( "LuaF%d", index )
	local rc, res = xllua.excel4( xlfRegister, 11, xllua.dll, proc, procTypes, name, nil, "1", category, nil, nil, nil, nil );
	if rc == 0 then
		xllua.funs[ index ] = { name = name, fn = fn }
	end	
	xllua.debug_printf( "reg: %d, %s\n", rc, stringit( res ) ); 
end 

function xllua.open( dll )
	xllua.dll = dll
	xllua.debug_printf( "xllua.opening... (%s)\n", stringit( dll ) )
	-- FIXME
	local res = dofile( "F:\\eclipse\\git\\xlloop.git\\xllua\\test\\addin1.lua" )
	xllua.debug_printf( "addin invoked: %s\n", stringit( res ) )
	return res or 1
end

function xllua.close()
	xllua.debug_printf( "xllua.closing...\n" )
	return 1
end

function xllua.fn( name, args )
	xllua.debug_printf( "xllua.invoken: %s %s \n", name, stringit( args ) )
	return 1
end

function xllua.fc( num, args )
	local fn = stringit( xllua.funs[ num ].name )
	xllua.debug_printf( "xllua.invokec: %d:%s %s \n", num, fn, stringit( args ) )
	return "hello " .. fn
end
