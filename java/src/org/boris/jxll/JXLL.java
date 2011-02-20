/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class JXLL
{
    private static IXLLHost callback;
    private static Addin current;
    private static Map libraries = new HashMap();

    public static void setCallback(IXLLHost callback) {
        JXLL.callback = callback;
    }

    public static Addin load(File xll) {
        return load(xll.getAbsoluteFile().toString());
    }

    public static Addin load(String filename) {
        long library = JNI.loadLibrary(filename);
        if (library == 0)
            return null;
        Addin a = new Addin(library, filename);
        synchronized (libraries) {
            current = a;
            libraries.put(filename, current);
            JNI.xlAutoOpen(library);
            current = null;
        }
        return a;
    }

    public static void dispose(Addin addin) {
        JNI.dispose(addin);
    }

    static int xlCallVer() {
        return 4;
    }

    static int excel4(int xlfn, XLOperHolder operRes, XLOper[] opers) {
        switch (xlfn) {
        case XLFunctionNumber.xlGetName:
            operRes.value = new XLOper(current.name);
            break;
        case XLFunctionNumber.xlfRegister:
            return registerFunction(opers);
        case XLCommandNumber.xlcAlert:
            System.out.println("Alert: " + opers[0].str);
            if (callback != null)
                return callback.excel4(xlfn, operRes, opers);
        default:
            if (callback != null)
                return callback.excel4(xlfn, operRes, opers);
            System.out.println("excel4 unhandled: " + xlfn);
        }
        return 32; // xlretFailed
    }

    private static int registerFunction(XLOper[] opers) {
        if (opers.length < 4) {
            return XLErrType.xlerrValue;
        }
        String name = opers[0].str;
        Addin addin = (Addin) libraries.get(name);
        if (addin == null) {
            return XLErrType.xlerrName;
        }

        addin.registerFunction(opers);
        return 0;
    }
}
