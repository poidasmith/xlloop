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
XL_TYPE_SREF = 9

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

class XLSRef
  attr_reader :col_first, :col_last, :rw_first, :rw_last
  def initialize(col_first, col_last, rw_first, rw_last)
    @col_first = col_first
    @col_last = col_last
    @rw_first = rw_first
    @rw_last = rw_last
  end
  
  def stream_xloper(io)
    io.putc(XL_TYPE_SREF)
    XLCodec.encodeInt(@col_first, io)
    XLCodec.encodeInt(@col_last, io)
    XLCodec.encodeInt(@rw_first, io)
    XLCodec.encodeInt(@rw_last, io)
  end
end

class Object
  def stream_xloper(io)
    self.to_s.stream_xloper(io)
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
      if self.length > 0
        io.write(self)
      end
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
             (e.length-1).times do
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

class FunctionContext
  attr_reader :caller, :sheet_name
  def initialize(caller, sheet_name)
    @caller = caller
    @sheet_name = sheet_name
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

class ReflectionHandler
  def initialize
    o = Object.new
    @baseMethods = [o.public_methods, o.singleton_methods].flatten
    @methods = Hash.new
  end
  
  def push(object, namespace="")
    methods = [object.public_methods, object.singleton_methods].flatten - @baseMethods
    methods.each { |s| 
      m = object.method(s)
      k = namespace + s
      v = @methods[k]
      if v == nil
        @methods[k] = m
      elsif v.kind_of?(Array)
        v.push(m)
      else
        a = Array.new
        a.push(v)
        a.push(m)
        @methods[k] = a
      end
    }
  end
  
  def invoke(context, name, args)
    m = @methods[name]
    chomp(args) # remove trailing nulls
    if m == nil
      if name == "org.boris.xlloop.GetFunctions"
        fi = Array.new
        @methods.each_key { |k| fi.push(FunctionInformation.new(k)) }
        return fi
      else
        return "#Unknown Function"
      end
    elsif m.kind_of?(Array)
      m.each { |e| 
        if canInvoke(e, args.length)
          return invokeM(e, args)
        end
      }
      return "#Invalid number of arguments to #{e}"
    else
      if canInvoke(m, args.length)
        return invokeM(m, args)
      else 
        return "#Invalid number of arguments to #{m}"
      end
    end
  end
  
  protected
  def invokeM(method, args)
    begin
      method.call(*args)
    rescue
      return "#" + $!
    end
  end
  
  def canInvoke(method, argc)
    return (method.arity >= 0 && argc == method.arity) || 
     (method.arity < 0 && (method.arity + argc) >= -1)
  end
  
  def chomp(args)
    while args != nil && args.length > 0 && args.last == nil 
      args.pop
    end
  end
end

class XLCodec
  def XLCodec.decode(io)
    type = io.getc
    case type
      when XL_TYPE_NUM then return io.read(8).unpack('G')[0]
      when XL_TYPE_STR then return io.read(io.getc) 
      when XL_TYPE_BOOL then return io.getc == 0 ? false : true
      when XL_TYPE_ERR then return XLError.new(decodeInt(io))
      when XL_TYPE_MULTI then return decodeMulti(io)
      when XL_TYPE_MISSING then return nil
      when XL_TYPE_NIL then return nil
      when XL_TYPE_INT then return decodeInt(io)
      when XL_TYPE_SREF then return XLSRef.new(decodeInt(io), decodeInt(io), decodeInt(io), decodeInt(io))
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
      rows.times do
        arr2 = Array.new
        cols.times do 
          arr2.push(decode(io))
        end
        arr.push(arr2)
      end
    else
      rows.times do  
        arr.push(decode(io))
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
            context = nil
            name = XLCodec.decode(socket)
            if(!name.kind_of?(String))
              version = name
              if(version == 20)
                extra_info = XLCodec.decode(socket)
                if(extra_info)
                  caller = XLCodec.decode(socket)
                  sheet_name = XLCodec.decode(socket)
                  context = FunctionContext.new(caller, sheet_name)
                end
              else
                "#Unknown protocol version".stream_xloper(socket)
                socket.close
              end
              name = XLCodec.decode(socket)
            end
            argc = XLCodec.decode(socket)
            args = Array.new
            argc.times do
              args.push(XLCodec.decode(socket))
            end
            res = @handler.invoke(context, name, args)
            res.stream_xloper(socket)
            socket.flush
          rescue
            print "Error. ", $!, "\n"
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
