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

import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import org.boris.variant.Variant;

public interface RequestProtocol 
{
    public static final int REQ_TYPE_GENERIC = 0;
    public static final int REQ_TYPE_FUNCTION = 1; // Used for excel function
    
    public static final String TYPE_OK = "Ok";
    public static final String TYPE_ERROR = "Error";
    
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