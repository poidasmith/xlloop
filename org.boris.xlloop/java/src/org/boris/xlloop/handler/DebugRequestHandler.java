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

import org.boris.variant.VTMap;
import org.boris.variant.Variant;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestHandler;

public class DebugRequestHandler implements RequestHandler
{
    private RequestHandler handler;

    public DebugRequestHandler(RequestHandler handler) {
        this.handler = handler;
    }

    public Variant execute(String name, VTMap args) throws RequestException {
        System.out.println(name);
        System.out.println(args);
        Variant v = handler.execute(name, args);
        System.out.println(v);
        return v;
    }

    public boolean hasRequest(String name) {
        return handler.hasRequest(name);
    }
}
