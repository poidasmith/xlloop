package org.boris.functionserver.reflect;

import java.util.ArrayList;
import java.util.List;

import org.boris.functionserver.Function;
import org.boris.functionserver.RequestException;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTNull;
import org.boris.variantcodec.Variant;

public class OverloadedMethod implements Function 
{
    private List<InstanceMethod> methods = new ArrayList();

    public void add(InstanceMethod m) {
        this.methods.add(m);
    }
    
    public Variant execute(VTCollection args) throws RequestException {
        int lastArg = args.size() - 1;
        for(; lastArg >= 0; lastArg--) {
            if(!(args.get(lastArg) instanceof VTNull)) {
                break;
            }
        }
        for(InstanceMethod m : methods) {
            if(m.matchesArgs(args, lastArg)) {
                return m.execute(args);
            }
        }

        throw new RequestException("#Invalid args");
    }
}
