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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketException;

import org.boris.variantcodec.BinaryCodec;
import org.boris.variantcodec.VTLong;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.Variant;

public class BinaryRequestProtocol implements RequestProtocol
{
    private int lastType;
    private String lastName;

    public void send(Socket socket, String type, Variant data)
            throws IOException {
        OutputStream os = new BufferedOutputStream(socket.getOutputStream());
        BinaryCodec.encode(new VTString(type), os);
        BinaryCodec.encode(data, os);
        os.flush();
    }

    public void send(Socket socket, String type, String data)
            throws IOException {
        OutputStream os = new BufferedOutputStream(socket.getOutputStream());
        BinaryCodec.encode(new VTString(type), os);
        BinaryCodec.encode(new VTString(data), os);
        os.flush();
    }

    public Variant receive(Socket socket) throws IOException {
        InputStream is = new BufferedInputStream(socket.getInputStream());
        Variant v = BinaryCodec.decode(is);
        VTString t = null;
        if (v instanceof VTLong) {
            lastType = ((VTLong) v).intValue();
            t = (VTString) BinaryCodec.decode(is);
        } else if (v instanceof VTString) {
            lastType = REQ_TYPE_GENERIC;
            t = (VTString) v;
        }
        if (t == null)
            return null;
        lastName = t.get();
        return BinaryCodec.decode(is);
    }

    public String getLastName() {
        return lastName;
    }

    public boolean hasError() {
        return TYPE_ERROR.equals(lastName);
    }

    public void initialise(Socket socket) throws SocketException {
        socket.setKeepAlive(true);
        socket.setPerformancePreferences(0, 1, 0);
    }

    public void send(Socket socket, Exception error) throws IOException {
        send(socket, TYPE_ERROR, error.getMessage());
    }

    public int getLastType() {
        return lastType;
    }

    public void send(Socket socket, int type, String name, Variant data)
            throws IOException {
        OutputStream os = new BufferedOutputStream(socket.getOutputStream());
        BinaryCodec.encode(new VTLong(type), os);
        BinaryCodec.encode(new VTString(name), os);
        BinaryCodec.encode(data, os);
        os.flush();
    }
}
