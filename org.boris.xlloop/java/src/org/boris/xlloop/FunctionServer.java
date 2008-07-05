/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.boris.variant.VTCollection;
import org.boris.variant.VTMap;
import org.boris.variant.Variant;

public class FunctionServer
{
    private int port;
    private FunctionHandler fHandler;
    private RequestHandler rHandler;
    private ServerSocket socket;

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
    
    public void stop() throws IOException {
        socket.close();
    }

    public void run() throws IOException {
        socket = new ServerSocket(port);

        while (true) {
            new HandlerThread(socket.accept()).start();
        }
    }

    private class HandlerThread extends Thread
    {
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
                                (VTMap) msg);
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
