
require( "pack" )
local math, string, type, pack, print, tostring = math, string, type, pack, print, tostring;

module( "binarycodec" )
_VERSION = "1.0"

local xlTypeNum = 1
local xlTypeStr = 2
local xlTypeBool = 3
local xlTypeErr = 4
local xlTypeMulti = 5
local xlTypeMissing = 6
local xlTypeNil = 7
local xlTypeInt = 8
local xlTypeSRef = 9

local binarycodec = {}

local encoders = {
	number = function( value )
		return string.pack( "b>d", xlTypeNum, value )
	end,
	
	string = function( value )
		if( value:len() > 255 ) then
			value = value.sub( value, 1, 255 )
		end
		return string.pack( "bp", xlTypeStr, value )
	end,
	
	boolean = function( value )
		return string.pack( "bb", xlTypeBool, value and 1 or 0 )
	end,
	
	table = function( value )
		local cols = value[ "cols" ] or 1
		local rows = value[ "rows" ] or #value
		local res  = string.pack( "b>i2", xlTypeMulti, rows, cols )
		for i = 1, rows * cols do
			res = res .. binarycodec.encode( value[ i ] )
		end
		return res			
	end,
}

function binarycodec.encode(value)
	if( value == nill ) then
		return string.pack( "b", xlTypeNil )
	else
		return encoders[ type(value) ](value)
	end
end

function binarycodec.encodeInt(value)
	return string.pack( "b>i", xlTypeInt, value ) 
end

local decodeInt = function( buffer )
	local sz, val = string.unpack( buffer:receive( 4 ), ">i4" )
	return val
end

local decodeNop = function( buffer )
	return nil
end

local decoders = {
	[ xlTypeNum ] = function( buffer )
		local sz, val = string.unpack( buffer:receive( 8 ), ">d" )
		return val
	end,
	
	[ xlTypeStr ]  = function( buffer ) 
		return buffer:receive( string.byte( buffer:receive( 1 ) ) )
	end,
	
	[ xlTypeBool ] = function( buffer ) 
		return string.byte( buffer:receive( 1 ) ) ~= 0
	end,
	
	[ xlTypeErr ] = decodeInt,
	
	[ xlTypeMulti ] = function( buffer )
		local array = { rows = decodeInt( buffer ), cols = decodeInt( buffer ) }
		for i = 1, array.rows * array.cols do
			array[ i ] = binarycodec.decode( buffer )
		end 
		return array
	end,
	
	[ xlTypeMissing ] = decodeNop,
	[ xlTypeNil     ] = decodeNop,
	[ xlTypeInt     ] = decodeInt,	
}

function binarycodec.decode(buffer)
	local type = string.byte( buffer:receive( 1 ) )
	return decoders[ type ]( buffer )
end

return binarycodec
