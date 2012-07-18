
xllua = xllua or {}

--
-- Runtime options for the addin. Overwrite these when your addin intializes
--
xllua.options = {
	debug         = false, -- debug output to debug monitor
	general_fn    = "LF", -- the name of our general lua function
	general_fnv   = nil,  -- the name of our general (volatile) lua function
	convert_multi = true, -- disable to send multis as userdata
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

-- Index by number/name of our functions
xllua.funn = { }
xllua.func = { }

-- Used to registered functions that are executed via the LF function but 
-- aren't registered with Excel
xllua.dynf = { }

--
-- A utility printf that will print to the dbg monitor (eg sysinternals DebugView)
--
function xllua.debug_printf( fmt, ... )
	xllua.debug_print( string.format( fmt, ... ) )
end

-- 
-- Register a function with Excel
--   @name is the Excel function name
--   @fn is the function implementation
--   @options for function registration
-- 
function xllua.reg_fun( name, fn, options )
	local opt      = options or {}
	local category = opt.category or "Lua" 
	local index    = #xllua.func + 1
	local proc     = string.format( "LuaF%d", index )
	local rc, res  = xllua.excel4( xlfRegister, 11, xllua.dll, proc, procTypes, name, nil, "1", category, nil, nil, nil, nil );
	if rc == 0 then
		xllua.func[ index ] = { name = name,   fn = fn }
		xllua.funn[ name  ] = { index = index, fn = fn }
	end	
	if xllua.options.debug then
		xllua.debug_printf( "register( %s::%s as %s ) = %d, %s\n", stringit( category ), stringit( name ), stringit( proc ), rc, stringit( res ) );
	end 
	return rc == 0 and index or -1
end 

--
-- Register multiple functions at once
--
function xllua.reg_funs( fn_table, options )
	local ft  = fn_table or {}
	local opt = options or {}
	local res = {}
	for i, v in pairs( ft ) do
		res[ i ] = xllua.reg_fun( i, v, options )
	end
	return res
end

--
-- Register multiple dynamic functions
--   @dyn_table is name -> fn
--
function xllua.reg_dyns( dyn_table )
	for i, v in pairs( dyn_table ) do
		xllua.funn[ i ] = { fn = v }
	end
end

--
-- Called by the addin from xlAutoOpen (when Excel asks it to initilize)
-- We look from a lua file with the same filename/path (sans extension) as the
-- addin and call dofile on this.
--
function xllua.open( dll )
	xllua.debug_printf( "xllua.opening... ( %s )\n", stringit( dll ) )
	xllua.dll = dll
	local lua = string.sub( dll, 1, string.len( dll ) - 4 ) .. ".lua";
	if xllua.file_exists( lua ) then
		local f, error = loadfile( lua )
		if f ~= nil then
			local res, err = pcall( f )
			if not res then
				xllua.debug_printf( "%s\n", stringit( err ) )
			end
			return res 
		else
			xllua.debug_printf( "error: %s\n", error )
		end
	end	
	return 0
end

--
-- Called by the addin from xlAutoClose. Redefine this function to perform
-- any cleanup required
--
function xllua.close()
	if xllua.debug then
		xllua.debug_printf( "xllua.closing...\n" )
	end
	return 1
end

--
-- Our general named function 
--   @name is the function name
--   @args the args passed to the function
--
function xllua.fn( name, args )
	local res = "#Not implemented"
	local f   = xllua.funn[ name ] or xllua.dynf[ name ] or {}
	local fn  = f.fn
	if fn ~= nil then
		res = fn( args )
	end
	if xllua.options.debug then
		xllua.debug_printf( "xllua.fn: %s %s = %s\n", name, stringit( args ), stringit( res ) )
	end
	return res
end

--
-- Our indexed function
--   @num is the function index (returned from reg_fun)
--   @args the args passed to the function
--
function xllua.fc( num, args )
	local f    = xllua.func[ num ] or {}
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
