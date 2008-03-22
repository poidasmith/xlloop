package org.boris.functionserver.handler;

import org.boris.functionserver.RequestException;
import org.boris.functionserver.RequestHandler;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class EchoHandler implements RequestHandler {
    private int count;

    public Variant execute(VTStruct args) throws RequestException {
        args.add("count", ++count);
        return args;
    }
}
