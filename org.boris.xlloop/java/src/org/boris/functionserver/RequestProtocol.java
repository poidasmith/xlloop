package org.boris.functionserver;

import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import org.boris.variantcodec.Variant;

public interface RequestProtocol {
    public static final int REQ_TYPE_GENERIC = 0;
    public static final int REQ_TYPE_FUNCTION = 1; // Used for excel function
    
    public static final String TYPE_OK = "Ok";
    public static final String TYPE_ERROR = "Error";
    
    public static final boolean DEBUG = false;

    public abstract void initialise(Socket socket) throws SocketException;

    public abstract void send(Socket socket, String type, Variant data) throws IOException;

    public abstract void send(Socket socket, Exception error) throws IOException;

    public abstract void send(Socket socket, String type, String data) throws IOException;

    public abstract Variant receive(Socket socket) throws IOException;

    public abstract String getLastType();

    public abstract boolean hasError();

}