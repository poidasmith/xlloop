package org.boris.functionserver;

import java.io.IOException;
import java.net.Socket;

public class HandlerThread extends Thread {
    private Server server;
    private Socket socket;
    private RequestProtocol protocol = new BinaryRequestProtocol();

    public HandlerThread(Server server, Socket socket) {
        this.server = server;
        this.socket = socket;
    }

    public void run() {
        socket.setPerformancePreferences(0, 1, 0);
        while (!socket.isClosed()) {
            try {
                server.handleRequest(protocol, socket);
            } catch (IOException e) {
                e.printStackTrace();
                break;
            }
        }
    }
}