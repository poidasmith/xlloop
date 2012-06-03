
local xlloop = require( "xlloop" )

local fns = {}

function fns.echo(...)
	return {...}
end

xlloop.server()
