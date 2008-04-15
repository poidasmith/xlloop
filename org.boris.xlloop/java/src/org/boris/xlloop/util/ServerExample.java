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
import org.boris.xlloop.reflect.ReflectFunctionHandler;

public class ServerExample {
    public static void main(String[] args) throws Exception {
        // Create function server on the default port
        FunctionServer fs = new FunctionServer();
        
        // Create a reflection function handler and add the Math methods
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        rfh.addMethods("Math.", Math.class);
        
        // Create a function information handler to register our functions
        FunctionInformationRequestHandler firh = new FunctionInformationRequestHandler();
        firh.add(rfh.getFunctions());

        // Set the handlers
        fs.setRequestHandler(new DebugRequestHandler(firh));
        fs.setFunctionHandler(new DebugFunctionHandler(rfh));
        
        // Run the engine
        System.out.println("Listening on port 5454...");
        fs.run();
    }
}
