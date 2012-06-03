
local socket = require("socket")
local codec  = require("binarycodec")
local xlloop = require("xlloop")
require("base")

local test = function( client, name, args )
	local res = xlloop.invoke( client, name, args )
	print( prettytostring( res ) )

end

local client = socket.connect("127.0.0.1", 5454)
args = { "hello2", { 345, -12, 0.0004, true, false, { 777 } } }
test( client, "ArgsTest", args )
test( client, "BasicTypes", args )
for i = 1, 10 do
	test( client, "RandTest", {} )
end
test( client, "Test222", { string.rep( "asdf", 100 ) } )
client:close()





