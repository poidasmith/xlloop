
local codec = require( "binarycodec" )
local ipairs = ipairs

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

return xlloop