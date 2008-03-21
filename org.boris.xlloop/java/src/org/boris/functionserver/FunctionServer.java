package org.boris.functionserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class FunctionServer 
{
    private Map<String, RequestHandler> reqHandlers = new HashMap();
    private Map<String, FunctionHandler> funcHandlers = new HashMap();
    private int port;

    public FunctionServer(int port) {
        this.port = port;
    }
    
    public void run() throws IOException {
        ServerSocket ss = new ServerSocket(port);

        while (true) {
            new HandlerThread(this, ss.accept()).start();
        }
    }
    
    public void addRequestHandler(String name, RequestHandler handler) {
        reqHandlers.put(name, handler);
    }
    
    public void removeRequestHandler(String name) {
        reqHandlers.remove(name);
    }
    
    public void addFunctionHandler(String name, FunctionHandler handler) {
        funcHandlers.put(name, handler);
    }
    
    public void removeFunctionHandler(String name) {
        funcHandlers.remove(name);
    }

    void handleFunction(RequestProtocol protocol, Socket socket) throws IOException {
    }

    void handleRequest(RequestProtocol protocol, Socket socket)
            throws IOException {
        try {
            Variant msg = protocol.receive(socket);
            if (msg == null) {
                return;
            }
            if (protocol.hasError()) {
                throw new IOException("Protocol error: " + msg);
            }
            String type = protocol.getLastType();
            RequestHandler handler = reqHandlers.get(type);
            if (handler == null) {
                throw new RequestException("No handler found for: " + type);
            }
            VTStruct args = (VTStruct) msg;
            Variant res = handler.execute(args);
            protocol.send(socket, RequestProtocol.TYPE_OK, res);
        } catch (RequestException e) {
            protocol.send(socket, e);
        }
    }
}
