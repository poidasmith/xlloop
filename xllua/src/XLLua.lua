
xllua = xllua or {}
xllua.options = {
	debug       = true,
	general_fn  = "LF",
	general_fnv = nil,
}

function xllua.stringit( t ) -- should stick this somewhere common
	local res = ""
	if type( t ) == "table" then
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

local stringit    = xllua.stringit
local xlfRegister = 149
local procTypes   = "RPPPPPPPPPPPPPPPPPPPP"

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
	if xllua.options.debug then
		xllua.debug_printf( "register( %s, %s, %s ) = %d, %s\n", stringit( name ), stringit( category ), stringit( proc ), rc, stringit( res ) );
	end 
end 

function xllua.open( dll )
	if xllua.options.debug then
		xllua.debug_printf( "xllua.opening... (%s)\n", stringit( dll ) )
	end
	xllua.dll = dll
	local lua = string.sub( dll, 1, string.len( dll ) - 4 ) .. ".lua";
	local res = dofile( lua )
	return res or 1
end

function xllua.close()
	if xllua.debug then
		xllua.debug_printf( "xllua.closing...\n" )
	end
	return 1
end

function xllua.fn( name, args )
	local res = "#Not implemented"
	if xllua.options.debug then
		xllua.debug_printf( "xllua.fn: %s %s = %s\n", name, stringit( args ), stringit( res ) )
	end
	return res
end

function xllua.fc( num, args )
	local f    = xllua.funs[ num ] or {}
	local name = f.name
	local fn   = f.fn
	local res = nil;
	if name == nil then
		res = "#Unknown function"
	elseif fn == nil then
		res = "#Not implemented"
	else
		res = fn( args )
	end
	if xllua.options.debug then
		xllua.debug_printf( "xllua.fc: %s:%s %s = %s\n", stringit( num ), stringit( name ), stringit( args ), stringit( res ) )
	end
	return res	
end
