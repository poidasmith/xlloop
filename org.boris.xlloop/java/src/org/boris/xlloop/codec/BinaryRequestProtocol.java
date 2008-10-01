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

import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import org.boris.xlloop.RequestProtocol;
import org.boris.xlloop.xloper.XLoper;

public class BinaryRequestProtocol implements RequestProtocol
{
    public void initialise(Socket socket) throws SocketException {
    }

    public XLoper receive(Socket socket) throws IOException {
        return BinaryCodec.decode(socket.getInputStream());
    }

    public void send(Socket socket, XLoper data) throws IOException {
        BinaryCodec.encode(data, socket.getOutputStream());
    }
}
