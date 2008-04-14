package org.boris.xlloop.util;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;

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
