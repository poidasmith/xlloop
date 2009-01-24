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

import java.io.File;

import org.boris.jxll.Addin;
import org.boris.jxll.XLL;
import org.boris.xlloop.FunctionServer;
import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationFunctionHandler;
import org.boris.xlloop.util.FileSystemWatcher.CallbackAdaptor;

public class XLLServer
{
    private static String DIR = "F:/Development/xll/";

    // private static String DIR =
    // "F:\\eclipse\\workspace\\org.boris.jxll\\jni\\build\\TestXLL-Debug";

    public static void main(String[] args) throws Exception {
        final FunctionServer fs = new FunctionServer();
        final CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        final FunctionInformationFunctionHandler fifh = new FunctionInformationFunctionHandler();
        FileSystemWatcher fsw = new FileSystemWatcher(new File(DIR),
                new CallbackAdaptor() {
                    public void fileAdded(File f) {
                        if (f.getName().toLowerCase().endsWith("xll")) {
                            Addin a = XLL.load(f.getAbsolutePath());
                            if (a == null) {
                                System.out
                                        .println("Could not load addin: " + f);
                                return;
                            }
                            AddinFunctionHandler fh = new AddinFunctionHandler(
                                    a);
                            fifh.add(fh);
                            cfh.add(fh);
                            System.out.println("Loaded: " + f);
                        }
                    }
                });
        cfh.add(fifh);
        fsw.start();
        fs.setFunctionHandler(new DebugFunctionHandler(cfh));
        fs.run();
    }
}
