package org.boris.functionserver.handler;

import org.boris.functionserver.RequestException;
import org.boris.functionserver.RequestHandler;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class ReflectionHandler extends RequestHandler {

    public Variant execute(VTStruct args) throws RequestException {
        try {
            Class c = Class.forName(args.getString("class"));
        } catch (Exception e) {
            
        }
        return null;
    }
    
}
