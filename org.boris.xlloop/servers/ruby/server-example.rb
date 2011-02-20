require "xlloop"

class TestHandler
	def initialize
		@arrr = [('a'..'z'),('A'..'Z')].map{|i| i.to_a}.flatten
		@functionInfo = createFunctionInfo
	end
	
	def invoke(context, name, args)
	  print "Invoking #{name} ", args.inspect, "\n"
		case name
		when "RandTest" then # Test encoding of different types and different lengths
			a = Array.new
			(1 + rand(50)).times do 
				case rand(4)
				when 0 then a.push((0..50).map{ @arrr[rand(@arrr.length)]  }.join)
				when 1 then a.push(4000 * rand())
				when 2 then a.push(rand(2)==1 ? true : false)
				when 3 then a.push(rand(435345))
				end
			end
			return a
		when "ArrayTest" then # Test encoding of arrays
			a = Array.new
			for i in 1..15
				b = Array.new
				for j in 1..rand(10)
					b.push(j * i)
				end
				a.push(b)
			end
			return a
		when "ArgsTest" then
			return args[0]
    when "GetEmptyString" then
      return ""
		when "org.boris.xlloop.GetFunctions" then # Registers functions in excel 
			return @functionInfo
		else
			"#Unknown function"
		end
	end
	
	def createFunctionInfo
		a = Array.new
		a.push(FunctionInformation.new("RandTest", 
			"Test encoding of different types and different lengths"))
		a.push(FunctionInformation.new("ArrayTest", "Test encoding of arrays"))
		f3 = FunctionInformation.new("ArgsTest", "Testing inputting args")
		f3.addArg("anything", "Test this one")
		f3.addArg("else", "Whatever you like")
		f3.category = "Testing"
		a.push(f3)
		return a
	end
end

f = XLLoopServer.new(TestHandler.new, 5470)
puts "XLLoop Ruby Server Example..."
f.start
