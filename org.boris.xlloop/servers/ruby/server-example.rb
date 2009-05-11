require "xlloop"

class Handler
	def invoke(name, *args)
		puts "Invoke #{name} with #{args}"
		#34.3
		"Hello"
	end
end

f = XLLoopServer.new(Handler.new)
f.start
