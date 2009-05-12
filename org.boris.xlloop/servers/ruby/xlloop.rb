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
	
	def stream_xloper(io)
		io.putc(XL_TYPE_ERR)
		XLCodec.encodeInt(@err, io)
	end
end

class String
	def stream_xloper(io)
		io.putc(XL_TYPE_STR)
		if self.length > 255 
			io.putc(255)
			for i in 1..255 
				io.putc(self[i-1])
			end
		else
			io.putc(self.length & 0xff)
			io.write(self)
		end
	end
end

class Numeric
	def stream_xloper(io)
		io.putc(XL_TYPE_NUM)
		io.write([self].pack('G'))
	end
end

class Boolean
	def stream_xloper(io)
		io.putc(XL_TYPE_BOOL)
		io.putc(self ? 1 : 0)
	end
end

class TrueClass
	def stream_xloper(io)
		io.putc(XL_TYPE_BOOL)
		io.putc(1)
	end
end

class FalseClass
	def stream_xloper(io)
		io.putc(XL_TYPE_BOOL)
		io.putc(0)
	end
end

class NilClass
	def stream_xloper(io)
		io.putc(XL_TYPE_NIL)
	end
end

class Array
	def stream_xloper(io)
		io.putc(XL_TYPE_MULTI)
		XLCodec.encodeInt(self.length, io) # rows
		if self.length > 0 
			e = self[0]
			if e.kind_of?(Array) 
				XLCodec.encodeInt(e.length, io) # cols
				for i in 1..self.length
					elem = self[i-1]
					if elem.kind_of?(Array) 
						if elem.length == e.length 
							elem.each { |ee| ee.stream_xloper(io) }
						elsif elem.length < e.length 
							elem.each { |ee| ee.stream_xloper(io) }
							for j in elem.length..(e.length-1)
								io.putc(XL_TYPE_MISSING)
							end
						else
							for j in 1..e.length
								elem[j-1].stream_xloper(io)
							end
						end
					else
						elem.stream_xloper(io)
						for j in 2..e.length
							io.putc(XL_TYPE_MISSING)
						end
					end
				end
			else
				XLCodec.encodeInt(1, io)
				self.each { |elem| elem.stream_xloper(io) }
			end
		else
			XLCodec.encodeInt(0, io)
		end
	end
end

class Hash
	def stream_xloper(io)
		a = Array.new
		self.each { |k,v| 
			b = Array.new
			b.push(k)
			b.push(v)
			a.push(b)
		}
		a.stream_xloper(io)
	end
end

class FunctionInformation
	attr_reader :name, :help, :category, :shortcut, :topic
	attr_writer :help, :category, :shortcut, :topic
	
	def initialize(name, help=nil)
		@name = name
		@args = Array.new
		@argHelps = Array.new
		@help = help
		@category = nil
		@shortcut = nil
		@topic = nil
		@isVolatile = false
	end
	
	def addArg(name, help)
		@args.push(name)
		@argHelps.push(help)
	end
	
	def stream_xloper(io)
		h = Hash.new
		h["functionName"] = @name
		h["functionHelp"] = @help if @help != nil
		h["category"] = @category if @category != nil
		h["shortcutText"] = @shortcut if @shortcut != nil
		h["helpTopic"] = @topic if @topic != nil
		h["isVolatile"] = @isVolatile if @isVolatile
		if @args.length > 0
			h["argumentText"] = @args.join(",")
			h["argumentHelp"] = @argHelps
		end
		h.stream_xloper(io)
	end
end

class XLCodec
	def XLCodec.decode(io)
		type = io.getc
		case type
			when XL_TYPE_NUM then return io.read(8).unpack('G')
			when XL_TYPE_STR then return io.read(io.getc) 
			when XL_TYPE_BOOL then return io.getc ? true : false
			when XL_TYPE_ERR then return XLError.new(decodeInt(io))
			when XL_TYPE_MULTI then return decodeMulti(io)
			when XL_TYPE_MISSING then return nil
			when XL_TYPE_NIL then return nil
			when XL_TYPE_INT then return decodeInt(io)
			else raise "Invalid XLoper type"
		end
	end
	
	def XLCodec.decodeInt(io)
		io.getc << 24 | io.getc << 16 | io.getc << 8 | io.getc
	end
	
	def XLCodec.decodeMulti(io)
		rows = decodeInt(io)
		cols = decodeInt(io)
		arr = Array.new
		if cols > 1
			for i in 1..rows
				arr2 = Array.new
				for j in 1..cols
					arr2[j] = decode(io)
				end
				arr[i] = arr2
			end
		else
			for i in 1..rows 
				arr[i] = decode(io)
			end
		end
		return arr
	end
	
	def XLCodec.encodeInt(value, io)
		io.putc(value >> 24 & 0xff)
		io.putc(value >> 16 & 0xff)
		io.putc(value >> 8 & 0xff)
		io.putc(value & 0xff)
	end
end

class XLLoopServer
	def initialize(handler, port=5454)
		@handler = handler
		@port = port
	end
	
	def start
		@socket = TCPServer.new('localhost', @port)
		while !@socket.closed?
			Thread.start(@socket.accept) { |socket|
				while !socket.closed?
					begin
						name = XLCodec.decode(socket)
						argc = XLCodec.decode(socket)
						args = Array.new
						argc.times do
							args.push(XLCodec.decode(socket))
						end
						res = @handler.invoke(name, args)
						res.stream_xloper(socket)
						socket.flush
					rescue
						socket.close
					end
				end
			}
		end
	end
	
	def stop
		@socket.close
	end
end
