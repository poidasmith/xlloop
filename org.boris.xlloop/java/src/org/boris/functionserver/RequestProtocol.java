package org.boris.functionserver;

import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import org.boris.variantcodec.Variant;

public interface RequestProtocol 
{
    public static final int REQ_TYPE_GENERIC = 0;
    public static final int REQ_TYPE_FUNCTION = 1; // Used for excel function
    
    public static final String TYPE_OK = "Ok";
    public static final String TYPE_ERROR = "Error";
    
    public static final boolean DEBUG = true;

    public void initialise(Socket socket) throws SocketException;

    public void send(Socket socket, String name, Variant data) throws IOException;

    public void send(Socket socket, int type, String name, Variant data) throws IOException;

    public void send(Socket socket, Exception error) throws IOException;

    public void send(Socket socket, String type, String data) throws IOException;

    public Variant receive(Socket socket) throws IOException;

    public int getLastType(); 
    
    public String getLastName();

    public boolean hasError();
}