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

import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.xloper.XLList;
import org.boris.xlloop.xloper.XLoper;

public class DebugFunctionHandler implements FunctionHandler
{
    private FunctionHandler h;

    public DebugFunctionHandler(FunctionHandler h) {
        this.h = h;
    }

    public XLoper execute(String name, XLList args) throws RequestException {
        System.out.println(name + args);
        return h.execute(name, args);
    }

    public boolean hasFunction(String name) {
        return h.hasFunction(name);
    }
}
