package org.boris.functionserver;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.VTStruct;
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

    public Variant execute(String name, VTCollection args) throws RequestException, IOException {
        connect();
        protocol.send(socket, RequestProtocol.REQ_TYPE_FUNCTION, name, args);
        return receive();
    }

    public Variant execute(String name, VTStruct args) throws RequestException, IOException {
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
