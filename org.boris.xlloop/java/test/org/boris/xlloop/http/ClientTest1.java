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

import java.net.URL;

import org.boris.xlloop.xloper.XLoper;

public class ClientTest1
{
    public static String reqj = "{\"args\":[],\"name\":\"org.boris.xlloop.GetFunctions\",\"request\":\"XLLoop\",\"version\":\"0.1.0\"}";

    public static void main(String[] args) throws Exception {
        URL u = new URL("http://localhost:8000/");
        String n = "RandTest";
        XLoper res = FunctionExecutor.execute(u, n, null);
        System.out.println(JSONCodec.encode(res).toString(4));
    }
}
