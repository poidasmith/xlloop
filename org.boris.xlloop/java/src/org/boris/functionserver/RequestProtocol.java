package org.boris.functionserver;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.Socket;
import java.net.SocketException;

import org.boris.variantcodec.Codec;
import org.boris.variantcodec.Variant;

public class RequestProtocol {
    public static final String TYPE_OK = "Ok";
    public static final String TYPE_ERROR = "Error";
    private String lastType;

    public void initialise(Socket socket) throws SocketException {
        socket.setKeepAlive(true);
    }

    public void send(Socket socket, String type, Variant data)
            throws IOException {
        send(socket, type, Codec.encodeDefault(data));
    }

    public void sendQuietly(Socket socket, Exception error) {
        try {
            send(socket, error);
        } catch (Exception e) {
            e.printStackTrace();
            try {
                socket.close();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
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

    public void send(Socket socket, String type, String data)
            throws IOException {
        if (socket.isClosed()) {
            return;
        }
        lastType = type;
        OutputStream out = socket.getOutputStream();
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(out));
        bw.write(type);
        bw.newLine();
        bw.write(Integer.toString(data.length()));
        bw.newLine();
        bw.write(data);
        bw.write(0); // delimiter
        bw.flush();
    }

    public String receive(Socket socket) throws IOException {
        if (socket.isClosed()) {
            return null;
        }
        InputStream str = socket.getInputStream();
        BufferedReader br = new BufferedReader(new InputStreamReader(str));
        lastType = br.readLine();
        if (lastType == null) {
            socket.close();
            return null;
        }
        int size = Integer.parseInt(br.readLine());
        char[] buf = new char[size];
        br.read(buf);
        while (br.read() != 0)
            ; // read until delimiter
        return new String(buf);
    }

    public String getLastType() {
        return lastType;
    }

    public boolean hasError() {
        return lastType == null || TYPE_ERROR.equals(lastType);
    }
}
