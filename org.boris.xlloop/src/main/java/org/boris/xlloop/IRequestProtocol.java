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

import org.boris.xlloop.xloper.XLoper;

public interface IRequestProtocol
{
    public void initialise(Socket socket) throws IOException;

    public void send(Socket socket, XLoper data) throws IOException;

    public XLoper receive(Socket socket) throws IOException;
}