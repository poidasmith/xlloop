package org.boris.functionserver;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class EchoHandler implements RequestHandler {
    private int count;

    public Variant execute(String name, VTStruct args) throws RequestException {
        args.add("count", ++count);
        return args;
    }
}
