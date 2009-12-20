from xlloop import *
import string, random

class TestHandler:
    def invoke(self, context, name, args):
        if name == 'ArgsTest':
            return args
        elif name == 'RandTest':
            len = int(random.random() * 50 + 2)
            a = []
            for i in xrange(len):
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
        else:
            return [random.random() * 1000, 'Hello World!', 23.3, 11111]
        
h = TestHandler()
xs = XLLoopServer(h)
xs.start()

