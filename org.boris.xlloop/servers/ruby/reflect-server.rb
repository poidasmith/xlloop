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