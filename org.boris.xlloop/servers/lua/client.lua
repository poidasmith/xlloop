
local socket = require("socket")
local codec  = require("binarycodec")

function invoke(client, name, args)
	print(name)
	client:send(codec.encode(name))
	client:send(codec.encodeAsInt(#args))
	for k,v in ipairs(args) do
		print(k)
		print(v)
		client:send(codec.encode(v))
	end
	print("encoded... now decoding")
	return ( codec.decode(client) )
end

print("Hello")
print("there")

local client = socket.connect("127.0.0.1", 5454)

res = invoke(client, "Echo", { "hello2" })
print(res)



