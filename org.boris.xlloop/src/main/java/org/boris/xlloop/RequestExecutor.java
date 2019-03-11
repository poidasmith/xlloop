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
import java.net.InetAddress;
import java.net.Socket;

import org.boris.xlloop.codec.BinaryRequestProtocol;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

/**
 * A client for the request handler server.
 */
public class RequestExecutor
{
    private Socket socket;
    private InetAddress address;
    private int port;
    private IRequestProtocol protocol = new BinaryRequestProtocol();

    public RequestExecutor(InetAddress add, int port) {
        this.address = add;
        this.port = port;
    }

    public void connect() throws IOException {
        if (socket == null) {
            socket = new Socket(address, port);
            protocol.initialise(socket);
        }
    }

    public void disconnect() throws IOException {
        socket.close();
        socket = null;
    }

    public boolean isConnected() {
        return socket != null && socket.isConnected();
    }

    public XLoper execute(String name, XLoper[] args) throws RequestException, IOException {
        connect();
        protocol.send(socket, new XLString(name));
        protocol.send(socket, new XLInt(args.length));
        for (int i = 0; i < args.length; i++) {
            protocol.send(socket, args[i]);
        }
        return receive();
    }

    private XLoper receive() throws RequestException, IOException {
        return protocol.receive(socket);
    }
}
