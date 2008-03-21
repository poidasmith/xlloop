package org.boris.functionserver.protocol;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.Socket;
import java.net.SocketException;

import org.boris.functionserver.RequestProtocol;
import org.boris.variantcodec.BinaryCodec;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.Variant;

public class BinaryRequestProtocol implements RequestProtocol {
    private String lastType;

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
        VTString t = (VTString) BinaryCodec.decode(is);
        if (t == null)
            return null;
        lastType = t.get();
        return BinaryCodec.decode(is);
    }

    public String getLastType() {
        return lastType;
    }

    public boolean hasError() {
        return TYPE_ERROR.equals(lastType);
    }

    public void initialise(Socket socket) throws SocketException {
        socket.setKeepAlive(true);
        socket.setPerformancePreferences(0, 1, 0);
    }

    public void send(Socket socket, Exception error) throws IOException {
        StringBuilder sb = new StringBuilder();
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        error.printStackTrace(pw);
        sb.append(error.getMessage());
        sb.append("\n");
        sb.append(sw.toString());
        String s = sb.toString();
        send(socket, TYPE_ERROR, s);
    }
}
