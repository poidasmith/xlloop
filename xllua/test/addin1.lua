
--require( "base" )

function stringit( t ) -- should stick this somewhere common
	local res = ""
	if     type( t ) == "table" then
		res = "{"
		for i, v in pairs(t) do
			res = res .. " " .. stringit( i ) .. "=" .. stringit( v ) 
		end
		res = res .. " }"
	elseif type( t ) == "string" then
		res = string.format( "%q", t )
	else
		res = tostring( t )		
	end
	return res
end

local xlfRegister = 149

local procTypes = "RCPPPPPPPPPPPPPPPPPPPP"

xllua.funs = {}

function xllua.debug_printf( fmt, ... )
	xllua.debug_print( string.format( fmt, ... ) )
end

function xllua.reg_fun( dll, name, category, args )
	local index   = #xllua.funs
	local proc    = string.format( "LuaF%d", index )
	local rc, res = xllua.excel4( xlfRegister, 11, dll, proc, procTypes, name, nil, "1", category, nil, nil, nil, nil );
	if rc == 0 then
		xllua_funs[ index ] = name
	end	
	xllua.debug_printf( "reg: %d, %s\n", rc, stringit( res ) ); 
end 

function xllua.open( dll )
	xllua.debug_printf( "xllua.opening...%s\n", stringit( dll ) )
	-- register a fun for fun
	xllua.reg_fun( dll, "Lut", "Testing", nil )
	return 1
end

function xllua.close()
	xllua.debug_printf( "xllua.closing...\n" )
	return 1
end

function xlFn( name, args )
	xllua.debug_printf( "xllua.invokeN: %s %s \n", name, stringit( args ) )
	return 1
end

function xlFc( num, args )
	xllua.debug_printf( "xllua.invokeN: %d:%s %s \n", num, xllua.funs[ num ], stringit( args ) )
end