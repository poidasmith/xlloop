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

import org.boris.xlloop.FunctionServer;
import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationFunctionHandler;
import org.boris.xlloop.handler.GetLoadServerFunctionHandler;
import org.boris.xlloop.reflect.Reflect;
import org.boris.xlloop.reflect.ReflectFunctionHandler;

public class ServerExample
{
    private static final boolean LOAD_BALANCE = false;

    public static void main(String[] args) throws Exception {
        // Create function server on the default port
        FunctionServer fs = new FunctionServer();

        // Create a reflection function handler and add the Math methods
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        rfh.addMethods("Math.", Math.class);
        rfh.addMethods("Math.", Maths.class);
        rfh.addMethods("CSV.", CSV.class);
        rfh.addMethods("Reflect.", Reflect.class);

        // Create a function information handler to register our functions
        FunctionInformationFunctionHandler firh = new FunctionInformationFunctionHandler();
        firh.add(rfh.getFunctions());

        // Create a function handler to demonstrate the "load balancing"
        // capability
        GetLoadServerFunctionHandler glsfh = new GetLoadServerFunctionHandler();

        // Set the handlers
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        cfh.add(firh);
        if (LOAD_BALANCE)
            cfh.add(glsfh);
        fs.setFunctionHandler(new DebugFunctionHandler(cfh));

        // Run the engine
        System.out.println("Listening on port " + fs.getPort() + "...");
        fs.run();
    }
}
