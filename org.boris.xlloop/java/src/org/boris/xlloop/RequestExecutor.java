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

import org.boris.variant.VTCollection;
import org.boris.variant.VTMap;
import org.boris.variant.VTString;
import org.boris.variant.Variant;

/**
 * A client for the request handler server.
 */
public class RequestExecutor
{
    private Socket socket;
    private InetAddress address;
    private int port;
    private RequestProtocol protocol = new BinaryRequestProtocol();

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
    }

    public Variant execute(String name, VTCollection args)
            throws RequestException, IOException {
        connect();
        protocol.send(socket, RequestProtocol.REQ_TYPE_FUNCTION, name, args);
        return receive();
    }

    public Variant execute(String name, VTMap args) throws RequestException,
            IOException {
        connect();
        protocol.send(socket, name, args);
        return receive();
    }

    private Variant receive() throws RequestException, IOException {
        Variant msg = protocol.receive(socket);
        if (protocol.hasError()) {
            throw new RequestException(((VTString) msg).get());
        }

        return msg;
    }
}
