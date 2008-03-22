package org.boris.functionserver;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class ExecuteHandler implements FunctionHandler {

    public Variant execute(String name, VTCollection args) throws RequestException {
        System.out.println(name + args);

        if (args.size() > 0) {
            return args.get(0);
        }

        VTStruct s = new VTStruct();
        s.add("hello there", 123213);
        s.add("asdf", "woeiruewoir");
        return s;
    }

}
