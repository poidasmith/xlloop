package org.boris.functionserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;

import org.boris.functionserver.handler.EchoHandler;
import org.boris.functionserver.handler.ExecuteHandler;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class Server {
    private Map<String, RequestHandler> handlers = new HashMap();

    public static void main(String[] args) throws Exception {
        ServerSocket ss = new ServerSocket(5454);
        Server server = new Server();
        server.initialise();

        while (true) {
            new HandlerThread(server, ss.accept()).start();
        }
    }

    private void initialise() {
        handlers.put("Echo", new EchoHandler());
        handlers.put("Exec", new ExecuteHandler());
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
            RequestHandler handler = handlers.get(type);
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
