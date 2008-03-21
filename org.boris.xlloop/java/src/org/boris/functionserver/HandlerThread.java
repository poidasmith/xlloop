package org.boris.functionserver;

import java.io.IOException;
import java.net.Socket;

import org.boris.functionserver.protocol.BinaryRequestProtocol;

public class HandlerThread extends Thread {
    private FunctionServer server;
    private Socket socket;
    private RequestProtocol protocol = new BinaryRequestProtocol();

    public HandlerThread(FunctionServer server, Socket socket) {
        this.server = server;
        this.socket = socket;
    }

    public void run() {
        socket.setPerformancePreferences(0, 1, 0);
        while (!socket.isClosed()) {
            try {
                server.handleRequest(protocol, socket);
            } catch (IOException e) {
                System.err.println(e.getMessage());
                break;
            }
        }
    }
}