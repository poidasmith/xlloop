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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.boris.xlloop.FunctionServer.HandlerThread;
import org.boris.xlloop.handler.DebugFunctionHandler;

public class TestMultipleServers
{
    public static void main(String[] args) throws IOException {
        final Map serverHandlers = new HashMap();
        FunctionServer[] servers = new FunctionServer[5];
        for (int i = 0; i < servers.length; i++) {
            final FunctionServer server = createServer(5454 + i);
            servers[i] = server;
            server.start();
            serverHandlers.put(server, new HashSet());
            server.setListener(new IFunctionServerListener() {
                public void connectionCreated(HandlerThread ht) {
                    ((HashSet) serverHandlers.get(server)).add(ht);
                }
            });
        }

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            String line = br.readLine().trim().toLowerCase();
            try {
                if (line.startsWith("start ")) {
                    int s = Integer.parseInt(line.substring(6));
                    servers[s].start();
                    System.out.println("Starting server " + s);
                } else if (line.startsWith("stop ")) {
                    int s = Integer.parseInt(line.substring(5));
                    servers[s].stop();
                    Set<HandlerThread> hs = (Set<HandlerThread>) serverHandlers.get(servers[s]);
                    for (HandlerThread ht : hs) {
                        ht.close();
                    }
                    hs.clear();
                    System.out.println("Stopping server " + s);
                } else if (line.equals("quit")) {
                    break;
                } else {
                    System.out.println("Unknown command: " + line);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static FunctionServer createServer(int port) {
        FunctionServer fs = ServerTest1.createServer(port);
        DebugFunctionHandler dfh = (DebugFunctionHandler) fs.getFunctionHandler();
        dfh.setLabel("[" + fs.getPort() + "] ");
        return fs;
    }
}
