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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.boris.xlloop.IFunctionHandler;

public class FunctionServlet extends HttpServlet
{
    private IFunctionHandler handler;

    public void setHandler(IFunctionHandler handler) {
        this.handler = handler;
    }

    public void init(ServletConfig config) throws ServletException {
        String cls = config.getInitParameter("FunctionServlet.handlerClass");
        try {
            handler = (IFunctionHandler) Class.forName(cls).newInstance();
        } catch (Exception e) {
            throw new ServletException(e);
        }
    }

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        doPost(req, resp);
    }

    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        try {
            BufferedReader input = new BufferedReader(new InputStreamReader(req.getInputStream()));
            BufferedWriter output = new BufferedWriter(new OutputStreamWriter(resp.getOutputStream()));
            JSONCodec.doRequest(handler, input, output);
        } catch (Exception e) {
            resp.sendError(400, "Bad Request");
        }
    }
}
