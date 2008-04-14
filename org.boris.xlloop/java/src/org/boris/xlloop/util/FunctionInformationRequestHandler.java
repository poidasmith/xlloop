package org.boris.xlloop.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestHandler;

public class FunctionInformationRequestHandler implements RequestHandler
{
    private ArrayList functions = new ArrayList();
    
    public void add(FunctionInformation fi) {
        functions.add(fi);
    }
    
    public void add(FunctionInformation[] fis) {
        functions.addAll(Arrays.asList(fis));
    }
    
    public Variant execute(String name, VTStruct args) throws RequestException {
        VTCollection c = new VTCollection();
        for(Iterator i = functions.iterator(); i.hasNext(); ) {
            FunctionInformation fi = (FunctionInformation) i.next();
            c.add(fi.encode());
        }
        return c;
    }

    public boolean hasRequest(String name) {
        return "GetFunctions".equals(name);
    }
}
