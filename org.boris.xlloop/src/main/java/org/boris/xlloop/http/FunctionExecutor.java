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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.HttpURLConnection;
import java.net.URL;

import org.boris.xlloop.util.IO;
import org.boris.xlloop.xloper.XLoper;

public class FunctionExecutor
{
    public static XLoper execute(URL u, String name, XLoper[] args) throws IOException {
        HttpURLConnection huc = (HttpURLConnection) u.openConnection();
        huc.setRequestMethod("GET");
        huc.setDoOutput(true);
        huc.connect();
        FunctionRequest fr = new FunctionRequest(name, args, null, null);
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(huc.getOutputStream()));
        StringWriter debug = new StringWriter();
        try {
            JSONCodec.encodeRequest(fr, debug);
            System.out.println(debug.toString());
            JSONCodec.encodeRequest(fr, bw);
            bw.flush();
            bw.close();
            StringWriter sw = new StringWriter();
            IO.copy(new InputStreamReader(huc.getInputStream()), sw, false);
            System.out.println(sw.toString());
            return JSONCodec.decodeXLoper(new StringReader(sw.toString()));
        } catch (Exception e) {
            e.printStackTrace();
            StringWriter sw = new StringWriter();
            IO.copy(new InputStreamReader(huc.getErrorStream()), sw, false);
            System.out.println(sw.toString());
            return null;
        } finally {
            huc.disconnect();
        }
    }
}
