package org.boris.functionserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class FunctionServer {
    private int port;
    private FunctionHandler fHandler;
    private RequestHandler rHandler;

    public FunctionServer() {
        this(5454);
    }

    public FunctionServer(int port) {
        this.port = port;
    }

    public FunctionServer(int port, FunctionHandler f, RequestHandler r) {
        this.port = port;
        this.fHandler = f;
        this.rHandler = r;
    }

    public void setFunctionHandler(FunctionHandler h) {
        this.fHandler = h;
    }

    public void setRequestHandler(RequestHandler h) {
        this.rHandler = h;
    }

    public void run() throws IOException {
        ServerSocket ss = new ServerSocket(port);

        while (true) {
            new HandlerThread(ss.accept()).start();
        }
    }

    private class HandlerThread extends Thread {
        private Socket socket;
        private RequestProtocol protocol = new BinaryRequestProtocol();

        public HandlerThread(Socket socket) {
            this.socket = socket;
        }

        public void run() {
            socket.setPerformancePreferences(0, 1, 0);
            while (!socket.isClosed()) {
                try {
                    Variant msg = protocol.receive(socket);
                    if (msg == null) {
                        throw new IOException("Protocol error: " + msg);
                    }

                    Variant res = null;
                    switch (protocol.getLastType()) {
                    case RequestProtocol.REQ_TYPE_GENERIC:
                        res = rHandler.execute(protocol.getLastName(),
                                (VTStruct) msg);
                        break;
                    case RequestProtocol.REQ_TYPE_FUNCTION:
                        res = fHandler.execute(protocol.getLastName(),
                                (VTCollection) msg);
                        break;
                    default:
                        throw new IOException("Unexpected protocol type: " +
                                protocol.getLastType());
                    }

                    protocol.send(socket, RequestProtocol.TYPE_OK, res);
                } catch (RequestException e) {
                    try {
                        protocol.send(socket, e);
                    } catch (IOException ex) {
                        System.err.println(e.getMessage());
                        break;
                    }
                } catch (Throwable e) {
                    System.err.println(e.getMessage());
                    try {
                        socket.close();
                    } catch (IOException ex) {
                    }
                    break;
                }
            }
        }
    }
}
