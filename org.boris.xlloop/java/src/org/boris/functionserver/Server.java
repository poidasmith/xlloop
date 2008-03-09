package org.boris.functionserver;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;

import org.boris.functionserver.handler.EchoHandler;
import org.boris.variantcodec.Codec;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class Server {
    private Map<String, RequestHandler> handlers = new HashMap();

    public static void main(String[] args) throws Exception {
        ServerSocket ss = new ServerSocket(5454);
        Server server = new Server();
        server.initialise();

        while (true) {
            Socket s = ss.accept();
            // Only accept sockets from same machine for now
            if (s.getInetAddress() == null ||
                    !s.getInetAddress().equals(InetAddress.getLocalHost())) {
                s.close();
            }
            HandlerThread h = new HandlerThread(server, s);
            h.start();
        }
    }

    private void initialise() {
        handlers.put("Echo", new EchoHandler());
    }

    void handleRequest(RequestProtocol protocol, Socket socket)
            throws IOException {
        try {
            String msg = protocol.receive(socket);
            if (msg == null) {
                return;
            }
            // System.out.println(protocol.getLastType());
            // System.out.println(msg);
            if (protocol.hasError()) {
                throw new IOException("Protocol error: " + msg);
            }
            String type = protocol.getLastType();
            RequestHandler handler = handlers.get(type);
            if (handler == null) {
                throw new RequestException("No handler found for: " + type);
            }
            VTStruct args = (VTStruct) Codec.decode(msg);
            Variant res = handler.execute(args);
            protocol.send(socket, RequestProtocol.TYPE_OK, res);
        } catch (RequestException e) {
            protocol.send(socket, e);
        }
    }
}
