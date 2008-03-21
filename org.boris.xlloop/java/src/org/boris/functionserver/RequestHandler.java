package org.boris.functionserver;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public abstract class RequestHandler 
{
    public abstract Variant execute(VTStruct args) throws RequestException;
}
