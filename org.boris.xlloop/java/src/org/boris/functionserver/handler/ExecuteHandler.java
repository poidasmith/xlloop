package org.boris.functionserver.handler;

import org.boris.functionserver.RequestException;
import org.boris.functionserver.RequestHandler;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class ExecuteHandler extends RequestHandler {

    public Variant execute(VTStruct args) throws RequestException {
        String function = args.getString("name");
        VTCollection fargs = args.getCollection("args");
        System.out.println(fargs);
        if (function == null) {
            throw new RequestException("Function name not specified");
        }
        return new VTString("Hello World!");
    }

}
