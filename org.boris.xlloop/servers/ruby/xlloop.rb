require "socket"

#  Defines the XLoper types
XL_TYPE_NUM = 1
XL_TYPE_STR = 2
XL_TYPE_BOOL = 3
XL_TYPE_ERR = 4
XL_TYPE_MULTI = 5
XL_TYPE_MISSING = 6
XL_TYPE_NIL = 7
XL_TYPE_INT = 8

# Defines XLError types
XL_ERROR_NULL = 0
XL_ERROR_DIV0 = 7
XL_ERROR_VALUE = 15
XL_ERROR_REF = 23
XL_ERROR_NAME = 29
XL_ERROR_NUM = 36
XL_ERROR_NA = 42

class XLError
	attr_reader :err
	def initialize(err)
		@err = err
	end
end

class XLCodec
	def XLCodec.decode(socket)
		type = socket.getc
		case type
			when XL_TYPE_NUM then return socket.read(8).unpack('E')[0]
			when XL_TYPE_STR  
				len = socket.getc
				puts len
				socket.read(len)
			when XL_TYPE_BOOL then return socket.getc ? true : false
			when XL_TYPE_ERR then return XLError.new(decodeInt(socket))
			when XL_TYPE_MULTI then return decodeMulti(socket)
			when XL_TYPE_MISSING then return nil
			when XL_TYPE_NIL then return nil
			when XL_TYPE_INT then return decodeInt(socket)
			else raise "Invalid XLoper type"
		end
	end
	
	def XLCodec.decodeInt(socket)
		return socket.read(4).unpack('v')[0]
	end
	
	def XLCodec.decodeMulti(socket)
		rows = decodeInt(socket)
		cols = decodeInt(socket)
		arr = Array.new
		if cols > 1
			for i in 1..rows
				arr2 = Array.new
				for j in 1..cols
					arr2[j] = decode(socket)
				end
				arr[i] = arr2
			end
		else
			for i in 1..rows 
				arr[i] = decode(socket)
			end
		end
		return arr
	end
	
	def XLCodec.encode(value, socket)
		case value
		
			when value == nil 
				socket.putc(XL_TYPE_NIL)
			when value === Numeric 
				socket.putc(XL_TYPE_NUM)
				socket.send([value].pack('E'))
			when value === String
				socket.putc(XL_TYPE_STR)
				if value.length > 255
					socket.putc(255)
					for i in 1..255 
						socket.putc(value[i])
					end
				else
					socket.putc(value.length)
					socket.send(value)
				end
			when value === XLError
				socket.putc(XL_TYPE_ERR)
				socket.send([value.err].pack('v'))
			when value === Array
				socket.putc(XL_TYPE_MULTI)
			else
				puts "Unhandled type #{value.type}"
				socket.putc(XL_TYPE_ERR)
				socket.putc(XL_ERROR_NIL)
		end
	end
end

class XLLoopServer
	def initialize(port=5454)
		@port = port
	end
	
	def start
		@socket = TCPServer.new('localhost', @port)
		loop {
			Thread.start(@socket.accept) { |s|
				loop {
					name = XLCodec.decode(s)
					puts name
					argc = XLCodec.decode(s)
					puts argc
					args = Array.new
					for i in 1..argc
						args.push(XLCodec.decode(s))
						puts args[i]
					end
					# Todo invoke function name/args
					XLCodec.encode("Hello World!", s)
				}
			}
		}
	end
	
	def stop
		@socket.shutdown(2)
		@socket.close_write
		@socket.close_read
		@socket = nil
	end
end
