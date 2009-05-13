import socket, select, struct, threading, SocketServer

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

class XLError:
    def __init__(self, err):
        self.err = err
    
class XLCodec:
    def decode(socket):
        type = socket.recv(1)
        if type == XL_TYPE_NUM:
            return unpack('>d', recv(8))
        elif type == XL_TYPE_STR:
            len = socket.recv(1)
            return socket.recv(len)
        elif type == XL_TYPE_BOOL:
            return True if socket.recv(1) else False
        elif type == XL_TYPE_ERR:
            return XLError(unpack('>i', socket.recv(4)))
        elif type == XL_TYPE_MULTI:
            rows = unpack('>i', socket.recv(4))
            cols = unpack('>i', socket.recv(4))
            
        elif type == XL_TYPE_MISSING:
            return null
        elif type == XL_TYPE_NIL:
            return null
        elif type == XL_TYPE_INT:
            return unpack('>i', socket.recv(4))
        else:
            raise TypeError("Invalid XLoper type encountered")
    decode = staticmethod(decode)
    
    def encode(value, socket):
        pass
    encode = staticmethod(encode)

class XLLoopHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        while 1:
            try:
                name = XLCodec.decode(self.request)
                argc = XLCodec.decode(self.request)
                args = []
                for i in 1..argc:
                    args.append(XLCodec.decode(self.request))
                res = self.server.handler.invoke(name, args)
                XLCodec.encode(res, self.request)
                self.request.flush()
            except BaseException:
                self.request.shutdown
                break
            
class XLLoopServer:
    def __init__(self, handler, port=5454):
        self.handler = handler
        self.port = port
        
    def start(self):
        self.server = SocketServer.ThreadingTCPServer(('localhost', self.port), XLLoopHandler)
        self.server.handler = self.handler
        self.server.serve_forever()
    
    def stop(self):
        self.server.shutdown()
    
class TestHandler:
    def invoke(self, name, args):
        if name=='ArgsTest':
            return args
        
h = TestHandler()
xs = XLLoopServer(h)
xs.start()



        