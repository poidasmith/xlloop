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
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import org.boris.xlloop.IFunctionHandler;

import se.rupy.http.Event;
import se.rupy.http.Service;

public class FunctionService extends Service
{
    private String path;
    private IFunctionHandler handler;

    public FunctionService(String path, IFunctionHandler handler) {
        this.path = path;
        this.handler = handler;
    }

    public void filter(Event event) throws Event, Exception {
        event.reply().header("Cache-Control", "no-cache");
        BufferedReader input = new BufferedReader(new InputStreamReader(event.input()));
        BufferedWriter output = new BufferedWriter(new OutputStreamWriter(event.output()));
        try {
            JSONCodec.doRequest(handler, input, output);
        } catch (Exception e) {
            e.printStackTrace();
            event.reply().code("400 Bad Request");
        }
    }

    public String path() {
        return path;
    }
}
