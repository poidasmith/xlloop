package org.boris.functionserver;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import org.boris.variantcodec.BinaryCodec;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.Variant;

public class BinaryRequestProtocol extends RequestProtocol {
    
    public void send(Socket socket, String type, Variant data) throws IOException {
        OutputStream os = new BufferedOutputStream(socket.getOutputStream());
        BinaryCodec.encode(new VTString(type), os);
        BinaryCodec.encode(data, os);
        os.flush();
    }
    
    public void send(Socket socket, String type, String data) throws IOException {
        OutputStream os = new BufferedOutputStream(socket.getOutputStream());
        BinaryCodec.encode(new VTString(type), os);
        BinaryCodec.encode(new VTString(data), os);
        os.flush();
    }
    
    public Variant receive(Socket socket) throws IOException {
        InputStream is = new BufferedInputStream(socket.getInputStream());
        lastType = ((VTString) BinaryCodec.decode(is)).get();
        return BinaryCodec.decode(is); 
    }
}
