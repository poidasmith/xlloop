from xlloop import *

class TestHandler:
    def invoke(self, name, args):
        if name == 'ArgsTest':
            return args
        elif name == 'RandTest':
            return [self, 'Hello', random.random()]
        else:
            return [random.random() * 1000, 'Hello World!', 23.3, 11111]
        
h = TestHandler()
xs = XLLoopServer(h)
xs.start()

