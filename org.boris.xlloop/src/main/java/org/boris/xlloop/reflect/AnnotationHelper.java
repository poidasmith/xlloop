/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.reflect;

import org.boris.xlloop.handler.FunctionInformation;

public class AnnotationHelper
{
    public static FunctionInformation extract(String name, XLFunction xf) {
        if (xf == null)
            return null;
        String fn = xf.name();
        if (hasValue(fn))
            name = fn;
        FunctionInformation fi = new FunctionInformation(name);
        if (xf.isVolatile())
            fi.setVolatile(true);
        if (hasValue(xf.category()))
            fi.setCategory(xf.category());
        if (hasValue(xf.help()))
            fi.setFunctionHelp(xf.help());
        String[] args = xf.args();
        String[] argHelp = xf.argHelp();
        if (args != null && args.length > 0) {
            for (int i = 0; i < args.length; i++) {
                String arg = args[i];
                String help = null;
                if (argHelp != null && argHelp.length > i)
                    help = argHelp[i];
                if (hasValue(arg))
                    fi.addArgument(arg, help);
            }
        }
        return fi;
    }

    private static boolean hasValue(String str) {
        return str != null && str.length() > 0;
    }
}
