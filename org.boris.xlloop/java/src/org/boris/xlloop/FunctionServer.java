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

import org.boris.xlloop.codec.BinaryRequestProtocol;
import org.boris.xlloop.xloper.XLError;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class FunctionServer
{
    protected int port;
    protected FunctionHandler handler;
    protected ServerSocket socket;

    public FunctionServer() {
        this(5454);
    }

    public FunctionServer(int port) {
        this.port = port;
    }

    public int getPort() {
        return socket == null ? port : socket.getLocalPort();
    }

    public FunctionServer(int port, FunctionHandler f) {
        this.port = port;
        this.handler = f;
    }

    public void setFunctionHandler(FunctionHandler h) {
        this.handler = h;
    }

    public void stop() throws IOException {
        if (socket != null)
            socket.close();
        socket = null;
    }

    public void start() {
        if (socket != null)
            return;

        Thread t = new Thread(new Runnable() {
            public void run() {
                try {
                    FunctionServer.this.run();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });
        t.setName("XLLoop Function Server");
        t.setDaemon(true);
        t.start();
    }

    public void run() throws IOException {
        if (socket == null)
            socket = new ServerSocket(port);

        while (true) {
            new HandlerThread(socket.accept()).start();
        }
    }

    private void handleRequest(RequestProtocol protocol, Socket socket) {
        try {
            XLString name = (XLString) protocol.receive(socket);
            XLInt argCount = (XLInt) protocol.receive(socket);
            XLoper[] args = new XLoper[argCount.w];
            for (int i = 0; i < argCount.w; i++) {
                args[i] = protocol.receive(socket);
            }
            XLoper res = handler.execute(name.str, args);
            if (res == null)
                res = XLError.NULL;
            protocol.send(socket, res);
        } catch (RequestException e) {
            try {
                protocol.send(socket, new XLString(e.getMessage()));
            } catch (IOException ex) {
                System.err.println(e.getMessage());
                try {
                    socket.close();
                } catch (IOException iex) {
                }
            }
        } catch (Throwable e) {
            System.err.println(e.getMessage());
            try {
                socket.close();
            } catch (IOException ex) {
            }
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
            try {
                protocol.initialise(socket);
            } catch (IOException e) {
            }
            while (!socket.isClosed()) {
                handleRequest(protocol, socket);
            }
        }
    }
}
