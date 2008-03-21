package org.boris.functionserver;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

import org.boris.functionserver.protocol.BinaryRequestProtocol;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.Variant;

public class RequestExecutor {
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

    public Variant execute(Request request) throws RequestException,
            IOException {
        connect();
        protocol.send(socket, request.getType(), request.getArgs());
        Variant msg = protocol.receive(socket);
        if (protocol.hasError()) {
            throw new RequestException(((VTString)msg).get());
        }

        return msg;
    }
}
