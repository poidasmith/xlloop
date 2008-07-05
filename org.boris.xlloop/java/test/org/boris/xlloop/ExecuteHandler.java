package org.boris.xlloop;

import org.boris.variant.VTCollection;
import org.boris.variant.VTMap;
import org.boris.variant.Variant;
import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;

public class ExecuteHandler implements FunctionHandler {

    public Variant execute(String name, VTCollection args) throws RequestException {
        System.out.println(name + args);

        if (args.size() > 0) {
            return args.get(0);
        }

        VTMap s = new VTMap();
        s.add("hello there", 123213);
        s.add("asdf", "woeiruewoir");
        return s;
    }

    public boolean hasFunction(String name) {
        return true;
    }

}
