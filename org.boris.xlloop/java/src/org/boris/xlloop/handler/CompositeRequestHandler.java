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

import org.boris.variantcodec.VTMap;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestHandler;

public class CompositeRequestHandler implements RequestHandler
{
    List handlers = new ArrayList();
    
    public void add(RequestHandler handler) {
        handlers.add(handler);
    }
    
    public Variant execute(String name, VTMap args) throws RequestException {
        for (Iterator i = handlers.iterator(); i.hasNext();) {
            RequestHandler h = (RequestHandler) i.next();
            if (h.hasRequest(name)) {
                return h.execute(name, args);
            }
        }

        throw new RequestException("#Unknown method: " + name);
    }

    public boolean hasRequest(String name) {
        for (Iterator i = handlers.iterator(); i.hasNext();) {
            RequestHandler h = (RequestHandler) i.next();
            if (h.hasRequest(name)) {
                return true;
            }
        }

        return false;
    }    
}
