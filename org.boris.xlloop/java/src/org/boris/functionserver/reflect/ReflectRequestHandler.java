package org.boris.functionserver.reflect;

import org.boris.functionserver.RequestMap;

public class ReflectRequestHandler extends RequestMap
{
    public ReflectRequestHandler(ReflectFunctionHandler handler) {
        add("GetFunctions", new GetFunctionsRequest(handler));
    }
}
