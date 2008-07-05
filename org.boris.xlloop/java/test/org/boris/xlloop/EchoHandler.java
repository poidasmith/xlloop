package org.boris.xlloop;

import org.boris.variant.VTMap;
import org.boris.variant.Variant;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestHandler;

public class EchoHandler implements RequestHandler {
    private int count;

    public Variant execute(String name, VTMap args) throws RequestException {
        args.add("count", ++count);
        return args;
    }

    public boolean hasRequest(String name) {
        return true;
    }
}
