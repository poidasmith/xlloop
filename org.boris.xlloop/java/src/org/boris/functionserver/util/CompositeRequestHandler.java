package org.boris.functionserver.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.boris.functionserver.RequestException;
import org.boris.functionserver.RequestHandler;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class CompositeRequestHandler implements RequestHandler
{
    List handlers = new ArrayList();
    
    public void add(RequestHandler handler) {
        handlers.add(handler);
    }
    
    public Variant execute(String name, VTStruct args) throws RequestException {
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
