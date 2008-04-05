package org.boris.functionserver.reflect;

import org.boris.functionserver.Request;
import org.boris.functionserver.RequestException;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class GetFunctionsRequest implements Request
{
    private ReflectFunctionHandler handler;

    public GetFunctionsRequest(ReflectFunctionHandler handler) {
        this.handler = handler;
    }

    public Variant execute(VTStruct args) throws RequestException {
        return handler.getFunctions();
    }
}
