/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.codec;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.Socket;

import org.boris.xlloop.IRequestProtocol;
import org.boris.xlloop.xloper.XLoper;

public class BinaryRequestProtocol implements IRequestProtocol
{
    private BufferedInputStream input;
    private BufferedOutputStream output;

    public void initialise(Socket socket) throws IOException {
        socket.setPerformancePreferences(0, 1, 0);
        input = new BufferedInputStream(socket.getInputStream());
        output = new BufferedOutputStream(socket.getOutputStream());
    }

    public XLoper receive(Socket socket) throws IOException {
        return BinaryCodec.decode(input);
    }

    public void send(Socket socket, XLoper data) throws IOException {
        BinaryCodec.encode(data, output);
        output.flush();
    }
}
