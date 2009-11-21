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
import org.boris.xlloop.util.XLMap;
import org.boris.xlloop.xloper.XLoper;

public class GetLoadServerFunctionHandler implements IFunctionHandler
{
    public static final String NAME = "org.boris.xlloop.GetLoadServer";

    // Expected args are [username,hostname] for client
    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        XLMap m = new XLMap();
        m.add("host", "localhost");
        m.add("port", 6000);
        return m.toXloper();
    }

    public boolean hasFunction(String name) {
        return NAME.equals(name);
    }
}
