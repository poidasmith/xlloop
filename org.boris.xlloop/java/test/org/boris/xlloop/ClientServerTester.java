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
import java.net.InetAddress;
import java.net.ServerSocket;

import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationFunctionHandler;
import org.boris.xlloop.reflect.ReflectFunctionHandler;
import org.boris.xlloop.xloper.XLoper;

public class ClientServerTester extends FunctionServer
{
    private CompositeFunctionHandler cfh;
    private ReflectFunctionHandler rfh;
    private RequestExecutor re;

    public ClientServerTester() throws Exception {
        super(0);
        cfh = new CompositeFunctionHandler();
        rfh = new ReflectFunctionHandler();
        cfh.add(rfh);
        FunctionInformationFunctionHandler fifh = new FunctionInformationFunctionHandler();
        fifh.add(rfh);
        cfh.add(fifh);
        socket = new ServerSocket(getPort());
        setFunctionHandler(new DebugFunctionHandler(cfh));
        start();
        re = new RequestExecutor(InetAddress.getLocalHost(), getPort());
        re.connect();
    }

    public void addMethods(String namespace, Class clazz) {
        rfh.addMethods(namespace, clazz);
    }

    public XLoper exec(String name, XLoper[] args) throws Exception {
        return re.execute(name, args);
    }

    public XLoper exec(String name, XLoper arg0) throws Exception {
        return re.execute(name, new XLoper[] { arg0 });
    }

    public void start() {
        Thread t = new Thread(new Runnable() {
            public void run() {
                try {
                    ClientServerTester.this.run();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });
        t.setName("XLLoop Function Server");
        t.setDaemon(true);
        t.start();
    }

    public void stop() throws IOException {
        super.stop();
        re.disconnect();
    }
}
