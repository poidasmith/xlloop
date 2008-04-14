package org.boris.xlloop.util;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestHandler;

public class DebugRequestHandler implements RequestHandler
{
    private RequestHandler handler;

    public DebugRequestHandler(RequestHandler handler) {
        this.handler = handler;
    }

    public Variant execute(String name, VTStruct args) throws RequestException {
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
