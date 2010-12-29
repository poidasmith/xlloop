import random, socket, select, struct, threading, traceback, types, SocketServer
import inspect

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

class XLError:
    def __init__(self, err):
        self.err = err

class XLSRef:
    def __init__(self, col_first, col_last, rw_first, rw_last):
        self.col_first = col_first
        self.col_last = col_last
        self.rw_first = rw_first
        self.rw_last = rw_last

class XLCodec:
    def decode(socket):
        type = ord(socket.recv(1))
        if type == XL_TYPE_NUM:
            return struct.unpack('>d', socket.recv(8))[0]
        elif type == XL_TYPE_STR:
            len = ord(socket.recv(1))
            if len == 0:
                return ''
            return socket.recv(len)
        elif type == XL_TYPE_BOOL:
            if ord(socket.recv(1)) == 0:
                return False
            else:
                return True
        elif type == XL_TYPE_ERR:
            return XLError(XLCodec.decodeInt(socket))
        elif type == XL_TYPE_MULTI:
            rows = XLCodec.decodeInt(socket)
            cols = XLCodec.decodeInt(socket)
            if cols == 0 or rows == 0:
                return []
            a = []
            if cols > 1:
                for i in xrange(rows):
                    aa = []
                    for j in xrange(cols):
                        aa.append(XLCodec.decode(socket))
                    a.append(aa)
            else:
                for i in xrange(rows):
                    a.append(XLCodec.decode(socket))
            return a
        elif type == XL_TYPE_MISSING:
            return None
        elif type == XL_TYPE_NIL:
            return None
        elif type == XL_TYPE_INT:
            return XLCodec.decodeInt(socket)
        elif type == XL_TYPE_SREF:
            return XLSRef(XLCodec.decodeInt(socket), XLCodec.decodeInt(socket), XLCodec.decodeInt(socket), XLCodec.decodeInt(socket))
        else:
            raise TypeError("Invalid XLoper type encountered")
    decode = staticmethod(decode)

    def decodeInt(socket):
        l = socket.recv(4)
        return ord(l[0]) << 24 | ord(l[1]) << 16 | ord(l[2]) << 8 | ord(l[3])
    decodeInt = staticmethod(decodeInt)

    def encode(value, socket):
        if isinstance(value, types.StringType):
            socket.send(struct.pack('B', XL_TYPE_STR))
            l = len(value)
            socket.send(struct.pack('B', l))
            if l > 0:
                socket.send(value)
        elif isinstance(value, types.FloatType):
            socket.send(struct.pack('B', XL_TYPE_NUM))
            socket.send(struct.pack('>d', value))
        elif isinstance(value, types.NoneType):
            socket.send(struct.pack('B', XL_TYPE_NIL))
        elif isinstance(value, XLError):
            socket.send(struct.pack('B', XL_TYPE_ERR))
            socket.send(struct.pack('>i', value.err))
        elif isinstance(value, XLSRef):
            socket.send(struct.pack('B', XL_TYPE_SREF))
            socket.send(struct.pack('>i', value.col_first))
            socket.send(struct.pack('>i', value.col_last))
            socket.send(struct.pack('>i', value.rw_first))
            socket.send(struct.pack('>i', value.rw_last))
        elif isinstance(value, types.BooleanType):
            socket.send(struct.pack('B', XL_TYPE_BOOL))
            if(value == True):
                socket.send(struct.pack('B', 1))
            else:
                socket.send(struct.pack('B', 0))
        elif isinstance(value, types.IntType):
            socket.send(struct.pack('B', XL_TYPE_NUM))
            socket.send(struct.pack('>d', float(value)))
        elif isinstance(value, types.ListType):
            socket.send(struct.pack('B', XL_TYPE_MULTI))
            rows = len(value)
            socket.send(struct.pack('>i', rows))
            if rows == 0:
                socket.send(struct.pack('>i', 0)) # zero cols
            else:
                v = value[0]
                if isinstance(v, types.ListType):
                    cols = len(v)
                    socket.send(struct.pack('>i', cols))
                    for i in xrange(rows):
                        v = value[i]
                        if isinstance(v, types.ListType):
                            l = len(v)
                            if l < cols:
                                for j in xrange(l):
                                    XLCodec.encode(v[j], socket)
                                for j in xrange(l, cols):
                                    XLCodec.encode(None, socket)
                            else:
                                for j in xrange(cols):
                                    XLCodec.encode(v[j], socket)
                        else:
                            XLCodec.encode(v, socket)
                            for j in xrange(1, cols):
                                XLCodec.encode(None, socket)
                else:
                    socket.send(struct.pack('>i', 1))
                    for i in xrange(rows):
                        XLCodec.encode(value[i], socket)
        elif isinstance(value, types.TupleType):
            socket.send(struct.pack('B', XL_TYPE_MULTI))
            rows = len(value)
            socket.send(struct.pack('>i', rows))
            if rows == 0:
                socket.send(struct.pack('>i', 0))
            else:
                socket.send(struct.pack('>i', 1)) # only one column for tuples
            for i in xrange(rows):
                XLCodec.encode(value[i], socket)
        else:
            XLCodec.encode(str(value), socket)
    encode = staticmethod(encode)

class FunctionContext:
    def __init__(self, caller, sheet_name):
        self.caller = caller
        self.sheet_name = sheet_name

class XLLoopHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        while 1:
            try:
                context = None
                name = XLCodec.decode(self.request)
                if isinstance(name, types.IntType):
                    version = name
                    if version == 20:
                        extra_info = XLCodec.decode(self.request)
                        if extra_info:
                            caller = XLCodec.decode(self.request)
                            sheet_name = XLCodec.decode(self.request)
                            context = FunctionContext(caller, sheet_name)
                    else:
                        raise TypeError("Unknown protocol version")
                    name = XLCodec.decode(self.request)
                argc = XLCodec.decode(self.request)
                args = []
                for i in xrange(argc):
                    args.append(XLCodec.decode(self.request))
                res = self.server.handler.invoke(context, name, args)
                XLCodec.encode(res, self.request)
            except:
                traceback.print_exc()
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

class FnServer(object):
    def __init__(self):
        self.exposed_fns = {}
        self._get_fns = []

    def add_fn(self, excel_name, fn_to_call, doc=None, arg_descrs=[], args='', register=True):
        self.exposed_fns[excel_name] = fn_to_call
        if register:
            excel_info = [
                    ["functionName", excel_name],
                    ["functionHelp", doc],
                    ["category", "PYTHON"],
                    ["argumentHelp", arg_descrs],
                    ["argumentText", args]
                ]
            self._get_fns.append(excel_info)

    def invoke(self, context, name, args):
        if name in self.exposed_fns:
            try:
                return self.exposed_fns[name](*args)
            except TypeError, msg:
                print msg
                return
        else:
            return XLError(XL_ERROR_NAME)

    def get_fns(self):
        return tuple(self._get_fns,)

fn_server = FnServer()
fn_server.add_fn("org.boris.xlloop.GetFunctions", fn_server.get_fns, register=False)

def expose(excel_name=None, arg_descrs=[]):
    def decorator(fn):
        xln = excel_name # Need local reference, otherwise get exception
        if xln is None:
            xln = fn.__name__
        d = fn.func_doc
        args = ", ".join([x.title() for x in inspect.getargspec(fn)[0]])
        argd = arg_descrs
        fn_server.add_fn(xln, fn, doc=d, arg_descrs=argd, args=args)
        return fn
    return decorator

def run(port=5454):
    print 'Starting Function Server on port %d...' %port
    xs = XLLoopServer(fn_server, port)
    xs.start()
