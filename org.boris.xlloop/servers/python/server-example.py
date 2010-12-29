from xlloop import *
import string, random, array

class TestHandler:
    def invoke(self, context, name, args):
        print(name)
        if name == 'ArgsTest':
            if(len(args) > 0):
                print(args[0])
                return args[0]
            else:
                return XLError(XL_ERROR_NA)
        
        elif name == 'RandTest':
            rlen = int(random.random() * 50 + 2)
            a = []
            for i in xrange(rlen):
                v = int(random.random() * 4)
                if v == 0:
                    a.append("".join(random.sample(string.letters+string.digits, 50)))
                elif v == 1:
                    a.append(int(random.random() * 1000))
                elif v == 2:
                    if int(random.random() * 2) == 1:
                        a.append(True)
                    else:
                        a.append(False)
                else:
                    a.append(random.random() * 1000)
            return a
        elif name == 'GetEmptyString':
            return ''
        else:
            return [random.random() * 1000, 'Hello World!', 23.3, 11111]
        
h = TestHandler()
xs = XLLoopServer(h, 5460)
xs.start()

