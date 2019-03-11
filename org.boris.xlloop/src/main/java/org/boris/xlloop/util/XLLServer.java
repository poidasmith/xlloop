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
import org.boris.jxll.JXLL;
import org.boris.xlloop.FunctionServer;
import org.boris.xlloop.handler.AddinFunctionHandler;
import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationHandler;
import org.boris.xlloop.util.FileSystemWatcher.CallbackAdaptor;

public class XLLServer
{
    private static String DIR = ".";

    public static void main(String[] args) throws Exception {
        System.out
                .println("XLLServer v0.0.1 - searching current directory for addins...");
        final FunctionServer fs = new FunctionServer();
        final CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        final FunctionInformationHandler fifh = new FunctionInformationHandler();
        FileSystemWatcher fsw = new FileSystemWatcher(new File(DIR),
                new CallbackAdaptor() {
                    public void fileAdded(File f) {
                        registerAddin(f, fifh, cfh);
                    }
                });
        cfh.add(fifh);
        fsw.setPauseMillis(1000);
        fsw.start();
        fs.setFunctionHandler(new DebugFunctionHandler(cfh));
        fs.run();
    }

    private static void registerAddin(File f,
            FunctionInformationHandler fifh,
            CompositeFunctionHandler cfh) {
        String n = f.getName().toLowerCase();
        if (n.indexOf("xlloop-") == -1 && n.endsWith("xll")) {
            Addin a = JXLL.load(f.getAbsolutePath());
            if (a == null) {
                System.out.println("Could not load addin: " + f);
                return;
            }
            AddinFunctionHandler fh = new AddinFunctionHandler(a);
            fifh.add(fh);
            cfh.add(fh);
            System.out.println("Loaded: " + f);
        }
    }
}
