package org.boris.functionserver.protocol;

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

import org.boris.functionserver.RequestProtocol;
import org.boris.variantcodec.StringCodec;
import org.boris.variantcodec.Variant;

public class StringRequestProtocol implements RequestProtocol {
    protected String lastType;

    /* (non-Javadoc)
     * @see org.boris.functionserver.IRequestProtocol#initialise(java.net.Socket)
     */
    public void initialise(Socket socket) throws SocketException {
        socket.setKeepAlive(true);
        socket.setPerformancePreferences(0,1,0);
    }

    /* (non-Javadoc)
     * @see org.boris.functionserver.IRequestProtocol#send(java.net.Socket, java.lang.String, org.boris.variantcodec.Variant)
     */
    public void send(Socket socket, String type, Variant data)
            throws IOException {
        send(socket, type, StringCodec.encodeDefault(data));
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

    /* (non-Javadoc)
     * @see org.boris.functionserver.IRequestProtocol#send(java.net.Socket, java.lang.Exception)
     */
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

    /* (non-Javadoc)
     * @see org.boris.functionserver.IRequestProtocol#send(java.net.Socket, java.lang.String, java.lang.String)
     */
    public void send(Socket socket, String type, String data)
            throws IOException {
        if (socket.isClosed()) {
            return;
        }
        lastType = type;
        OutputStream out = socket.getOutputStream();
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(out));
        if(DEBUG) {
            StringBuilder sb = new StringBuilder();
            sb.append(type);
            sb.append("\n");
            sb.append(Integer.toString(data.length()));
            sb.append("\n");
            sb.append(data);
            System.out.println(sb.toString());
            bw.write(sb.toString());
            bw.write(0); // delimiter
            bw.flush();
        } else {
            bw.write(type);
            bw.newLine();
            bw.write(Integer.toString(data.length()));
            bw.newLine();
            bw.write(data);
            bw.write(0); // delimiter
            bw.flush();
        }
    }

    /* (non-Javadoc)
     * @see org.boris.functionserver.IRequestProtocol#receive(java.net.Socket)
     */
    public Variant receive(Socket socket) throws IOException {
        if (socket.isClosed()) {
            return null;
        }
        InputStream str = socket.getInputStream();
        BufferedReader br = new BufferedReader(new InputStreamReader(str));
        lastType = br.readLine();
        if(DEBUG) {
            System.out.println(lastType);
        }
        if (lastType == null) {
            socket.close();
            return null;
        }
        int size = Integer.parseInt(br.readLine());
        if(DEBUG) {
            System.out.println(size);
        }
        char[] buf = new char[size];
        br.read(buf);
        while (br.read() != 0)
            ; // read until delimiter
        if(DEBUG) {
            System.out.println(new String(buf));
        }
        return StringCodec.decode(new String(buf));
    }

    /* (non-Javadoc)
     * @see org.boris.functionserver.IRequestProtocol#getLastType()
     */
    public String getLastType() {
        return lastType;
    }

    /* (non-Javadoc)
     * @see org.boris.functionserver.IRequestProtocol#hasError()
     */
    public boolean hasError() {
        return lastType == null || TYPE_ERROR.equals(lastType);
    }
}
