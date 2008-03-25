package org.boris.functionserver.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.boris.functionserver.FunctionHandler;
import org.boris.functionserver.RequestException;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

public class CompositeFunctionHandler implements FunctionHandler 
{
    private List handlers = new ArrayList();

    public void add(FunctionHandler h) {
        handlers.add(h);
    }

    public Variant execute(String name, VTCollection args) throws RequestException {
        for (Iterator i = handlers.iterator(); i.hasNext();) {
            FunctionHandler h = (FunctionHandler) i.next();
            if (h.hasFunction(name)) {
                return h.execute(name, args);
            }
        }

        throw new RequestException("#Unknown method: " + name);
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
