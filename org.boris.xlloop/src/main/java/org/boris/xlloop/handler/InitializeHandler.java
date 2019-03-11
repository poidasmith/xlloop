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

import org.boris.xlloop.IBuiltinFunctions;
import org.boris.xlloop.IFunctionContext;
import org.boris.xlloop.IFunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.xloper.XLoper;

public class InitializeHandler implements IFunctionHandler, IBuiltinFunctions
{
    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        // TODO: perform intiailization here
        return null;
    }

    public boolean hasFunction(String name) {
        return INITIALIZE.equals(name);
    }
}
