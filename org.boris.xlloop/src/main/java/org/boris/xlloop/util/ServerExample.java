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

import org.boris.xlloop.reflect.Reflect;

public class ServerExample
{
    public static void main(String[] args) throws Exception {
        // Create function server on the default port
        FunctionHandlerServer fhs = new FunctionHandlerServer();

        // Add methods and infor
        fhs.addMethods("Math.", Math.class);
        fhs.addMethods("Math.", Maths.class);
        fhs.addMethods("CSV.", CSV.class);
        fhs.addMethods("Reflect.", Reflect.class);
        fhs.addInfo(CSVFunctionInformationReader.read(ServerExample.class.getResourceAsStream("math.csv")));

        // Run the engine
        System.out.println("Listening on port " + fhs.getPort() + "...");
        fhs.run();
    }
}
