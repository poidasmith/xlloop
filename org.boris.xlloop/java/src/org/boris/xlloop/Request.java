package org.boris.xlloop;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public interface Request 
{
    Variant execute(VTStruct args) throws RequestException;
}
