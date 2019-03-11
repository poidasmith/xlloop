/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.io.IOException;

import org.boris.xlloop.FunctionServer;
import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformation;
import org.boris.xlloop.handler.FunctionInformationHandler;
import org.boris.xlloop.handler.FunctionProvider;
import org.boris.xlloop.reflect.DelegateFunctionHandler;
import org.boris.xlloop.reflect.ReflectFunctionHandler;

public class FunctionHandlerServer extends FunctionServer
{
    private CompositeFunctionHandler handler;
    private ReflectFunctionHandler rfh;
    private DelegateFunctionHandler dfh;
    private DebugFunctionHandler debug;
    private FunctionInformationHandler info;

    public FunctionHandlerServer() {
        this(5454, true);
    }

    public FunctionHandlerServer(int port, boolean debug) {
        super(port);
        this.handler = new CompositeFunctionHandler();
        this.debug = new DebugFunctionHandler(handler);
        this.rfh = new ReflectFunctionHandler();
        this.dfh = new DelegateFunctionHandler();
        this.info = new FunctionInformationHandler();
        setDebug(debug);
        handler.add(rfh);
        handler.add(dfh);
        handler.add(info);
        info.add(rfh);
        info.add(dfh);
    }

    public void setDebug(boolean debug) {
        setFunctionHandler(debug ? this.debug : handler);
    }

    public void addMethods(String namespace, Class clazz) {
        rfh.addMethods(namespace, clazz);
    }

    public void addDelegates(String namespace, Class clazz) {
        dfh.addMethods(namespace, clazz);
    }

    public void addInfo(FunctionProvider provider) {
        info.add(provider);
    }

    public void addInfo(FunctionInformation[] info) {
        this.info.add(info);
    }

    public void run() throws IOException {
        info.add(rfh.getFunctions());
        info.add(dfh.getFunctions());
        super.run();
    }
}
