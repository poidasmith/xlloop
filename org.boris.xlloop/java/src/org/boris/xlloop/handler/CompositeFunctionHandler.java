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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLoper;

public class CompositeFunctionHandler implements FunctionHandler
{
    private List handlers = new ArrayList();

    public void add(FunctionHandler h) {
        handlers.add(h);
    }

    public XLoper execute(String name, XLList args) throws RequestException {
        for (Iterator i = handlers.iterator(); i.hasNext();) {
            FunctionHandler h = (FunctionHandler) i.next();
            if (h.hasFunction(name)) {
                return h.execute(name, args);
            }
        }

        throw new RequestException("#Unknown function: " + name);
    }

    public boolean hasFunction(String name) {
        for (Iterator i = handlers.iterator(); i.hasNext();) {
            FunctionHandler h = (FunctionHandler) i.next();
            if (h.hasFunction(name)) {
                return true;
            }
        }

        return false;
    }
}
