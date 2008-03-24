package org.boris.functionserver;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

public interface FunctionHandler 
{
    Variant execute(String name, VTCollection args) throws RequestException;
    
    boolean hasFunction(String name);    
}
