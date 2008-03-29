package org.boris.functionserver.util;

import org.boris.functionserver.FunctionHandler;
import org.boris.functionserver.RequestException;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

public class DebugFunctionHandler implements FunctionHandler
{
    private FunctionHandler h;

    public DebugFunctionHandler(FunctionHandler h) {
        this.h = h;
    }

    public Variant execute(String name, VTCollection args) throws RequestException {
        System.out.println(name + args);
        return h.execute(name, args);
    }

    public boolean hasFunction(String name) {
        return h.hasFunction(name);
    }

}
