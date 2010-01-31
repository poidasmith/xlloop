/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.http;

import javax.servlet.http.HttpServlet;

import se.rupy.http.Event;
import se.rupy.http.Service;

public class ServletContainer extends Service
{
    private String path;
    private HttpServlet servlet;

    public ServletContainer(String path, HttpServlet servlet) {
        this.path = path;
        this.servlet = servlet;
    }

    public void filter(Event event) throws Event, Exception {
        event.reply().header("Cache-Control", "no-cache");
        servlet.service(new ServletRequest(event.input()), new ServletResponse(event.output()));
    }

    public String path() {
        return path;
    }
}
