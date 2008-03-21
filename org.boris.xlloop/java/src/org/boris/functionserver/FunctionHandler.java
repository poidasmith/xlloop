package org.boris.functionserver;

import org.boris.functionserver.xlobject.XLObject;
import org.boris.variantcodec.Variant;

public interface FunctionHandler 
{
    Variant execute(String name, XLObject[] args);
}
