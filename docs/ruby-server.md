# ![XLLoop](./logo.gif) XLLoop 
A Ruby Function Server

## About

Included in the download is a Ruby implementation of the XLLoop server process. For more information on Ruby try www.ruby-lang.org.

## Usage

The Ruby server implementation consists of a single file: xlloop.rb. The code listing for a reflection-based example server is as follows:

```ruby
require "xlloop"

class MyFuncs
	def sum(*a)
		a.flatten.compact.inject(0) { |b,i| b+i }
	end
	
	def product(a,b)
		a*b
	end
end

h = ReflectionHandler.new
h.push(MyFuncs.new, "MyFuncs.")
f = XLLoopServer.new(h)
f.start
```

This creates a new server (a socket listening on port 5454) and exposes to functions in Excel; MyFuncs.sum and MyFuncs.product.