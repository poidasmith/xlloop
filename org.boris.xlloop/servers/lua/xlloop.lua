
local socket = require( "socket" )
local codec  = require( "binarycodec" )
local ipairs = ipairs
local type   = type

local globals = _G

module( "xlloop" )
_VERSION = "1.0"

xlloop = {}

function xlloop.invoke(client, name, args)
	client:send( codec.encode( name ) )
	client:send( codec.encodeInt( #args ) )
	for k, v in ipairs( args ) do
		client:send( codec.encode( v ) )
	end
	return codec.decode( client )
end

-- TODO: multiple client support
function xlloop.server(table, port)
	local t = table or globals
	local p = port or 5454
	local server = socket.bind( "*", p )
	while 1 do
		local client = server:accept()
		while 1 do
			local name = codec.decode( client )
			if name == nil then
				break
			end
			local argc = codec.decode( client )
			local args = {}
			for i = 1, argc do
				args[ i ] = codec.decode( client )
			end
			local fn = t[ name ]
			local res = nil
			if type( fn ) ~= "function" then
				res = "#Unknown function"
			else
				res = t[ name ]( unpack( args ) )
			end
			local enc = codec.encode( res )
			client:send( enc )
		end
	end 
end

return xlloop