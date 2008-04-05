package org.boris.functionserver;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public interface RequestHandler 
{
    public Variant execute(String name, VTStruct args) throws RequestException;
    
    boolean hasRequest(String name);
}
