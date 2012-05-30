
--module("binarycodec")

local xlTypeNum = 1
local xlTypeStr = 2
local xlTypeBool = 3
local xlTypeErr = 4
local xlTypeMulti = 5
local xlTypeMissing = 6
local xlTypeNil = 7
local xlTypeInt = 8
local xlTypeSRef = 9

local encoders = {
	string = function( value )
		res = string.char(xlTypeStr)
		len = value:len()
		if( len > 255 ) then
			len = 255
		end
		return res .. string.char( len ) .. string.sub( value, 1, len )
	end
}

function encode(value)
	return encoders[ type(value) ](value)
end

function encodeAsInt(value)
	return string.char(
		xlTypeInt,
		math.floor( value / 256 ^ 3) % 256,
		math.floor( value / 256 ^ 2) % 256,
		math.floor( value / 256 ^ 1) % 256,
		math.floor( value / 256 ^ 0) % 256
	)
end

decoders = {
	[ xlTypeNum ] = function( buffer )
	end,
	[ xlTypeStr ] = function( buffer )
		print( "decode str: " )
		len = buffer:receive( 1 )
		len = len:byte()
		print( len )
		return buffer:receive( len )
	end
}

function decode(buffer)
	type = buffer:receive(1)
	type = type:byte();
	print( "found type: " .. string.format("%d", type ) )
	return decoders[ type ]( buffer )
end

return {
	encode      = encode,
	encodeAsInt = encodeAsInt,
	decode      = decode,
}
