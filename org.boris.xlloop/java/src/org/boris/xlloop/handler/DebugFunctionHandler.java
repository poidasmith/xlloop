/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.handler;

import org.boris.xlloop.IFunctionContext;
import org.boris.xlloop.IFunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.xloper.XLSRef;
import org.boris.xlloop.xloper.XLoper;

public class DebugFunctionHandler implements IFunctionHandler
{
    private IFunctionHandler h;
    private String label;

    public DebugFunctionHandler(IFunctionHandler h) {
        this.h = h;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        if (label != null)
            System.out.print(label);
        System.out.print(name + "(");
        for (int i = 0; i < args.length; i++) {
            if (i > 0)
                System.out.print(",");
            System.out.print(args[i]);
        }
        System.out.print(")");
        if (context != null) {
            XLSRef caller = context.getCaller();
            String sname = context.getSheetName();
            if (sname != null) {
                System.out.print(" ");
                System.out.print(sname);
            }
            if (caller != null) {
                System.out.print(" ");
                System.out.print(caller);
            }
        }
        System.out.print(" = ");
        XLoper res = h.execute(context, name, args);
        System.out.println(res);
        return res;
    }

    public boolean hasFunction(String name) {
        return h.hasFunction(name);
    }
}
