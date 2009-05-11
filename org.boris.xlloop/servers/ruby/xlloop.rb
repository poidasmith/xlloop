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
	
	def to_xloper(socket)
		socket.putc XL_TYPE_ERR
		XLCodec
	end
end

class String
	def to_xloper(socket)
		socket.putc(XL_TYPE_STR)
		if self.length > 255
			puts "writing long #{self.length}"
			socket.putc(255)
			for i in 1..255 
				socket.putc(self[i-1])
			end
		else
			puts "writing short #{self.length}"
			socket.putc(self.length && 0xff)
			socket.write(self)
		end
	end
end

class Numeric
	def to_xloper(socket)
		socket.putc(XL_TYPE_NUM)
		socket.write([self].pack('G'))
	end
end

class XLCodec
	def XLCodec.decode(socket)
		type = socket.getc
		case type
			when XL_TYPE_NUM then return socket.read(8).unpack('G')
			when XL_TYPE_STR then return socket.read(socket.getc) 
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
		socket.getc << 24 | socket.getc << 16 | socket.getc << 8 | socket.getc
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
	
	def XLCodec.encodeInt(value, socket)
		socket.putc(value >> 24 & 0xff)
		socket.putc(value >> 16 & 0xff)
		socket.putc(value >> 8 & 0xff)
		socket.putc(value & 0xff)
	end
end

class XLLoopServer
	def initialize(handler, port=5454)
		@handler = handler
		@port = port
	end
	
	def start
		@socket = TCPServer.new('localhost', @port)
		loop {
			Thread.start(@socket.accept) { |s|
				loop {
					name = XLCodec.decode(s)
					argc = XLCodec.decode(s)
					args = Array.new
					if argc > 0
						for i in 1..argc
							args.push(XLCodec.decode(s))
						end
					end
					res = @handler.invoke(name, args)
					res.to_xloper(s)
					s.flush
				}
			}
		}
	end
	
	def stop
		@socket.shutdown(2)
		@socket.close_write
		@socket.close_read
	end
end
