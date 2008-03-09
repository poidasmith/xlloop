package org.boris.functionserver;

import java.io.IOException;
import java.net.Socket;

public class HandlerThread extends Thread {
    private Server server;
    private Socket socket;

    public HandlerThread(Server server, Socket socket) {
        this.server = server;
        this.socket = socket;
    }

    public void run() {
        while (!socket.isClosed()) {
            try {
                server.handleRequest(socket);
            } catch (IOException e) {
                e.printStackTrace();
                break;
            }
        }
    }
}