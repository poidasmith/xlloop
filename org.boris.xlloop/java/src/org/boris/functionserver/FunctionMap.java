package org.boris.functionserver;

import java.util.HashMap;
import java.util.Map;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

public class FunctionMap implements FunctionHandler 
{
    private Map<String, Function> functions = new HashMap();

    public void add(String name, Function f) {
        this.functions.put(name, f);
    }

    public void remove(String name) {
        this.functions.remove(name);
    }

    public void clear() {
        this.functions.clear();
    }

    public Variant execute(String name, VTCollection args) throws RequestException {
        Function f = functions.get(name);
        if (f == null) {
            throw new RequestException("Unknown function: " + name);
        }
        return f.execute(args);
    }
}
